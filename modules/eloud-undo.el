
;; Speak undo  actions.

(require 'eloud-command-map)
(require 'eloud)


(defun eloud-undo-describe-position (pos)
  "Return a text description of the position POS, of the current buffer."
  (let* ((lns (count-lines (point-min) (point-max)))
         (ln (line-number-at-pos pos))
         (pct (/ (* ln 100.0) lns))
         (ln-beg (line-beginning-position))
         (col (- ln-beg pos)))
    (format "column %d line %d of %d (%f%%) position %d"
            col ln  lns pct pos)))

(defun eloud-undo-describe-region (beg end)
  "Return a text description of the region from BEG to END, of the current buffer, suitable for someone who can't see well, or at all."
  (let* ((lns (count-lines (point-min) (point-max)))
         (beg-ln (line-number-at-pos beg))
         (ln-beg (save-excursion
                   (goto-char beg)
                   (line-beginning-position)))
         (col-beg (- ln-beg beg))
         (end-ln (line-number-at-pos end))
         (ln-end (save-excursion
                   (goto-char end)
                   (line-beginning-position)))
         (col-end (- ln-end end)))
    (cond
     ((equal beg-ln end-ln)
      (format "from column %d to column %d of line %d" col-beg col-end beg-ln))
     (t
      (format "from column %d of line %d to column %d of line %d" col-beg beg-ln col-end end-ln)))))

(defun eloud-undo-primitive-undo (n list)
  "Like PRIMITIVE-UNDO, except talks."
  (let ((arg n)
        ;; In a writable buffer, enable undoing read-only text that is
        ;; so because of text properties.
        (inhibit-read-only t)
        ;; Don't let `intangible' properties interfere with undo.
        (inhibit-point-motion-hooks t)
        ;; We use oldlist only to check for EQ.  ++kfs
        (oldlist buffer-undo-list)
        (did-apply nil)
        (next nil))
    (while (> arg 0)
      (while (setq next (pop list))     ;Exit inner loop at undo boundary.
        (message "DLD| next: %S" next)
        ;; Handle an integer by setting point to that value.
        (pcase next
          ((pred integerp)
           (eloud-speak (concat "undo back to "
                                (eloud-undo-describe-position next)))
           (goto-char next))
          ;; Element (t . TIME) records previous modtime.
          ;; Preserve any flag of NONEXISTENT_MODTIME_NSECS or
          ;; UNKNOWN_MODTIME_NSECS.
          (`(t . ,time)
           ;; If this records an obsolete save
           ;; (not matching the actual disk file)
           ;; then don't mark unmodified.
           (when (or (equal time (visited-file-modtime))
                     (and (consp time)
                          (equal (list (car time) (cdr time))
                                 (visited-file-modtime))))
             (when (fboundp 'unlock-buffer)
               (unlock-buffer))
             (eloud-speak "undo setting buffer unmodified.")
             (set-buffer-modified-p nil)))
          ;; Element (nil PROP VAL BEG . END) is property change.
          (`(nil . ,(or `(,prop ,val ,beg . ,end) pcase--dontcare))
           (when (or (> (point-min) beg) (< (point-max) end))
             (error "Changes to be undone are outside visible portion of buffer"))
           (eloud-speak (format "undo inserting %S text property." val))
           (put-text-property beg end prop val))
          ;; Element (BEG . END) means range was inserted.
          (`(,(and beg (pred integerp)) . ,(and end (pred integerp)))
           ;; (and `(,beg . ,end) `(,(pred integerp) . ,(pred integerp)))
           (when (or (> (point-min) beg) (< (point-max) end))
             (error "Changes to be undone are outside visible portion of buffer"))
           ;; Set point first thing, so that undoing this undo
           ;; does not send point back to where it is now.
           (goto-char beg)
           (message "DLD| undo del buf substr %S" (buffer-substring beg end))
           (eloud-speak (concat
                         (format "undo deleting text %s " (buffer-substring beg end))
                         (eloud-undo-describe-region beg end)))
           (delete-region beg end))
          ;; Element (apply FUN . ARGS) means call FUN to undo.
          (`(apply . ,fun-args)
           (let ((currbuff (current-buffer)))
             (if (integerp (car fun-args))
                 ;; Long format: (apply DELTA START END FUN . ARGS).
                 (pcase-let* ((`(,delta ,start ,end ,fun . ,args) fun-args)
                              (start-mark (copy-marker start nil))
                              (end-mark (copy-marker end t)))
                   (when (or (> (point-min) start) (< (point-max) end))
                     (error "Changes to be undone are outside visible portion of buffer"))
                   (apply fun args) ;; Use `save-current-buffer'?
                   ;; Check that the function did what the entry
                   ;; said it would do.
                   (unless (and (= start start-mark)
                                (= (+ delta end) end-mark))
                     (error "Changes to be undone by function different from announced"))
                   (set-marker start-mark nil)
                   (set-marker end-mark nil))
               (apply fun-args))
             (unless (eq currbuff (current-buffer))
               (error "Undo function switched buffer"))
             (setq did-apply t)))
          ;; Element (STRING . POS) means STRING was deleted.
          (`(,(and string (pred stringp)) . ,(and pos (pred integerp)))
           (let ((valid-marker-adjustments nil)
                 (apos (abs pos)))
             (when (or (< apos (point-min)) (> apos (point-max)))
               (error "Changes to be undone are outside visible portion of buffer"))
             ;; Check that marker adjustments which were recorded
             ;; with the (STRING . POS) record are still valid, ie
             ;; the markers haven't moved.  We check their validity
             ;; before reinserting the string so as we don't need to
             ;; mind marker insertion-type.
             (while (and (markerp (car-safe (car list)))
                         (integerp (cdr-safe (car list))))
               (let* ((marker-adj (pop list))
                      (m (car marker-adj)))
                 (and (eq (marker-buffer m) (current-buffer))
                      (= apos m)
                      (push marker-adj valid-marker-adjustments))))
             ;; Insert string and adjust point
             (if (< pos 0)
                 (progn
                   (goto-char (- pos))
                   (insert string))
               (goto-char pos)
               (insert string)
               (goto-char pos))
             ;; Adjust the valid marker adjustments
             (dolist (adj valid-marker-adjustments)
               ;; Insert might have invalidated some of the markers
               ;; via modification hooks.  Update only the currently
               ;; valid ones (bug#25599).
               (if (marker-buffer (car adj))
                   (set-marker (car adj)
                               (- (car adj) (cdr adj)))))))
          ;; (MARKER . OFFSET) means a marker MARKER was adjusted by OFFSET.
          (`(,(and marker (pred markerp)) . ,(and offset (pred integerp)))
           (warn "Encountered %S entry in undo list with no matching (TEXT . POS) entry"
                 next)
           ;; Even though these elements are not expected in the undo
           ;; list, adjust them to be conservative for the 24.4
           ;; release.  (Bug#16818)
           (when (marker-buffer marker)
             (set-marker marker
                         (- marker offset)
                         (marker-buffer marker))))
          (_ (error "Unrecognized entry in undo list %S" next))))
        (setq arg (1- arg)))
      ;; Make sure an apply entry produces at least one undo entry,
      ;; so the test in `undo' for continuing an undo series
      ;; will work right.
      (if (and did-apply
             (eq oldlist buffer-undo-list))
        (setq buffer-undo-list
              (cons (list 'apply 'cdr nil) buffer-undo-list))))
  list)

(defun eloud-undo-primitive-undo-advice (&rest r)
  (advice-remove 'primitive-undo 'eloud-undo-primitive-undo-advice)
  (unwind-protect
      (progn
        (message "DLD| prim undo advice first arg: %S" (nth 0 r))
        (eloud-undo-primitive-undo (cadr r) (caddr r)))
    (advice-add 'primitive-undo :around 'eloud-undo-primitive-undo-advice)))

(defun eloud-undo-activate ()
  (interactive)
  (eloud-command-map-activate undo)
  (message "eloud undo is now ON"))

(defun eloud-undo-deactivate ()
  (interactive)
  (eloud-command-map-deactivate undo)
  (message "eloud undo is now OFF"))

(defun eloud-undo-toggle-activation ()
  "Toggle eloud undo support on and off."
  (if eloud-mode
      (eloud-undo-activate)
    (eloud-undo-deactivate)))

(provide 'eloud-comint)


(provide 'eloud-undo)


(define-eloud-command-map undo)
(define-eloud-command-map-advice
  undo
  'primitive-undo
  'eloud-undo-primitive-undo-advice
  :around)
(eloud-command-map-deactivate undo)
(add-hook 'eloud-mode-hook 'eloud-undo-toggle-activation)


;; eloud-undo.el ends here.
