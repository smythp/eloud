
;; This is so one can use non-arrow key movement while listening to
;; speech.  If we speak char due to C-f, C-b, C-n, C-p then that will
;; interrupt the text already being spoken (very annoying).  This way
;; using arrow key movement speaks chars, but non-arrow key movement
;; does not, so we can move around with non-arrow keys while listening
;; to speech, as well as cancel it easily, while somewhat indicating
;; point position, by using an arrow key.
;;
;; Another approch would be to have multiple speech servers and allow chars to be
;; spoken over any existing speech, maybe with existing speech's volume lowered,
;; different voices, pitches, speed or whatever distinguishes between simultaneous speakers.


(require 'eloud-command-map)
(require 'eloud)


(defcustom eloud-arrow-keys-whole-line-p nil
  "Whether or not eloud arrow keys reads the whole line when the
down arrow key is pressed resulting in movement to the beginning of a line
or whether the down arrow
simply speaks the character on which point lands (even if point lands
on the beginning of a line.)
See also ELOUD-ARROW-KEYS-TOGGLE-whole-LINE-MODE."
  :type '(boolean)
  :group 'eloud)

(defvar eloud-arrow-keys-existing-up-fx nil)
(defvar eloud-arrow-keys-existing-down-fx nil)
(defvar eloud-arrow-keys-existing-left-fx nil)
(defvar eloud-arrow-keys-existing-right-fx nil)

(defun eloud-arrow-keys-bounds-error-advice (&rest r)
  (interactive "^p")
  (condition-case err
      (let ((old-func (car r))
            (n (cadr r)))
        (funcall old-func n))
    (args-out-of-range
     (cond
      ((= (point) (point-min))
       (eloud-speak "Beginning of buffer"))
      ((= (point) (point-max))
       (eloud-speak "End of buffer"))
      (t
       (eloud-speak "Range error at point %S" (point)))))))

(defun eloud-arrow-keys-modal-down-char (&rest args)
  (interactive "P")
  (message "DLD| modal-down-char, toggle is %S" eloud-arrow-keys-whole-line-p)
  (condition-case err
      (prog1
          (apply #'next-line args)
        (cond
         ((and eloud-arrow-keys-whole-line-p (bolp))
          (eloud-rest-of-line (lambda (&rest q))))
         (t
          (eloud-character-at-point #'(lambda (&rest q))))))
    (args-out-of-range
     (eloud-speak "End of buffer"))))

(defun eloud-arrow-keys-up-char (&rest args)
  (interactive "P")
  (condition-case err
      (apply #'previous-line args)
    (args-out-of-range
     (eloud-speak "Beginning of buffer"))))

(defun eloud-arrow-keys-down-char (&rest args)
  (interactive "P")
  (condition-case err
      (prog1
          (apply #'next-line args)
        (cond
         ((and eloud-arrow-keys-whole-line-p (bolp))
          (eloud-rest-of-line #'(lambda (&rest q))))
         (t
          (eloud-character-at-point #'(lambda (&rest q))))))
    (args-out-of-range
     (eloud-speak "End of buffer"))))

(defun eloud-arrow-keys-left-char (&rest args)
  (interactive "P")
  (condition-case err
      (apply #'left-char args)
    (args-out-of-range
     (eloud-speak "Beginning of buffer"))))

(defun eloud-arrow-keys-right-char (&rest args)
  (interactive "P")
  (condition-case err
      (apply #'right-char args)
    (args-out-of-range
     (eloud-speak "End of buffer"))))

(defun eloud-arrow-keys-toggle-whole-line-mode ()
  (interactive)
  (setq eloud-arrow-keys-whole-line-p
        (not eloud-arrow-keys-whole-line-p)))

(defun eloud-arrow-keys-save-keydefs ()
  (setq eloud-arrow-keys-existing-up-fx (lookup-key global-map [up] t))
  (setq eloud-arrow-keys-existing-down-fx (lookup-key global-map [down] t))
  (setq eloud-arrow-keys-existing-left-fx (lookup-key global-map [left] t))
  (setq eloud-arrow-keys-existing-right-fx (lookup-key global-map [right] t)))

(defun eloud-arrow-keys-restore-keydefs ()
  (define-key global-map [up] eloud-arrow-keys-existing-up-fx)
  (define-key global-map [down] eloud-arrow-keys-existing-down-fx)
  ;;(define-key global-map [left] eloud-arrow-keys-existing-left-fx)
  ;;(define-key global-map [right] eloud-arrow-keys-existing-right-fx)
  ;;(global-unset-key [left])
  ;;(global-unset-key [right])
  )

(defun eloud-arrow-keys-override-keydefs ()
  (define-key global-map [up] 'previous-line)
  (define-key global-map [down] 'eloud-arrow-keys-modal-down-char)
  (define-key global-map [left]		'backward-char)
  (define-key global-map [right] 'forward-char))

(defun eloud-arrow-keys-activate ()
  (interactive)
  ;; NB:  We are running after eloud has already toggled on.  Override eloud advice for these two:
  (eloud-cm-deactivate-advice 'next-line 'eloud-rest-of-line :around)
  (eloud-cm-deactivate-advice 'previous-line 'eloud-rest-of-line :around)
  (eloud-arrow-keys-save-keydefs)
  (eloud-arrow-keys-override-keydefs)
  (eloud-command-map-activate arrow-keys)
  (message "DLD| eloud arrow keys ON"))

(defun eloud-arrow-keys-deactivate ()
  (interactive)
  (eloud-cm-activate-advice 'next-line 'eloud-rest-of-line :around)
  (eloud-cm-activate-advice 'previous-line 'eloud-rest-of-line :around)
  (eloud-arrow-keys-restore-keydefs)
  (eloud-command-map-deactivate arrow-keys)
  (message "DLD| eloud arrow keys OFF"))

(defun eloud-arrow-keys-toggle-activation ()
  "Toggle eloud arrow keys on and off.  This is installed as a normal hook
on the ELOUD-MODE-HOOK minor mode hook variable."
  (if eloud-mode
      (eloud-arrow-keys-activate)
    (eloud-arrow-keys-deactivate)))

(provide 'eloud-arrow-keys)


(define-eloud-command-map arrow-keys)
;; A) Remove eloud advice from next-line and previous-line and install eloud-arrow-keys advice.  This makes rest of line be char at point for up/down.
(define-eloud-command-map-advice arrow-keys 'previous-line 'eloud-character-at-point :around)
(define-eloud-command-map-advice arrow-keys 'next-line 'eloud-character-at-point :around)
(define-eloud-command-map-advice arrow-keys 'eloud-character-at-point 'eloud-arrow-keys-bounds-error-advice :around)
;; NB: Decided to rebind left/right to forward/backward-char instead of left/right-char, so we don't need to advise left/right-char anymore.
;;(define-eloud-command-map-advice arrow-keys 'left-char 'eloud-character-at-point :around)
;;(define-eloud-command-map-advice arrow-keys 'right-char 'eloud-character-at-point :around)
(eloud-command-map-deactivate arrow-keys)

(add-hook 'eloud-mode-hook 'eloud-arrow-keys-toggle-activation)


;; eloud-arrow-keys.el ends here.
