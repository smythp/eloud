;; Speak when killing a modified buffer.



(defun eloud-kill-modified-buffer-yes-or-no-p-advice (&rest r)
  (advice-remove 'yes-or-no-p 'eloud-kill-modified-buffer-yes-or-no-p-advice)
  (let* ((prompt (cadr r))
         (msgl (cond
                (prompt
                 (list (format "%s? (yes or no)" prompt)))
                (t
                 (list "(yes or no)")))))
    (apply #'eloud-speak msgl)
    (apply #'yes-or-no-p (cdr r))))

(defun eloud-kill-modified-buffer-kill-buffer-advice (&rest r)
  (unwind-protect
      (progn
        (advice-remove 'kill-buffer 'eloud-kill-modified-buffer-kill-buffer-advice)
        (advice-remove 'read-from-minibuffer 'eloud-read-minibuffer-prompt)
        (unwind-protect
            (progn
              (advice-add 'yes-or-no-p :around 'eloud-kill-modified-buffer-yes-or-no-p-advice)
              (apply (car r) (cdr r)))
          ;; NB: advice may or may not actually be there depending on if yes-or-no-p was subsequently called.
          (advice-remove 'yes-or-no-p 'eloud-kill-modified-buffer-yes-or-no-p-advice)))
    (advice-add 'read-from-minibuffer :around 'eloud-read-minibuffer-prompt)
    (advice-add 'kill-buffer :around 'eloud-kill-modified-buffer-kill-buffer-advice)))

(defun eloud-kill-modified-buffer-activate ()
  (interactive)
  (eloud-command-map-activate kill-modified-buffer)
  (message "eloud kill modified-buffer is now ON"))

(defun eloud-kill-modified-buffer-deactivate ()
  (interactive)
  (eloud-command-map-deactivate kill-modified-buffer)
  (message "eloud kill modified buffer is now OFF"))

(defun eloud-kill-modified-buffer-toggle-activation ()
  "Toggle eloud kill modified buffer support on and off."
  (if eloud-mode
      (eloud-kill-modified-buffer-activate)
    (eloud-kill-modified-buffer-deactivate)))


(provide 'eloud-kill-modified-buffer)

(define-eloud-command-map kill-modified-buffer)
(define-eloud-command-map-advice
  kill-modified-buffer
  'kill-buffer
  'eloud-kill-modified-buffer-kill-buffer-advice
  :around)
(eloud-command-map-deactivate kill-modified-buffer)
(add-hook 'eloud-mode-hook 'eloud-kill-modified-buffer-toggle-activation)


;; eloud-kill-modifiedbuffer.el ends here.
