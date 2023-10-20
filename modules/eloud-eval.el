;;
;; Speech support for eval-buffer, eval-region, et. al.

(require 'eloud-command-map)
(require 'eloud)


(defun eloud-eval-advice (&rest r)
  "Advise eval-buffer, eval-region, et. al. so that if evaluation
completes without any errors a success indication is spoken,
otherwise speak the error message and allow the debugger to be
entered by re-signalling the error."
  (unwind-protect
      (progn
        ;; NB: This needs to be documented as a design flaw -- need to
        ;; allow for auto-unadvise/readvise as part of macros in
        ;; eloud-command-map.  Anyway, remove this advice from all
        ;; advisees.  We do this due to paranoia about runaway recursion.
        (advice-remove 'eval-buffer 'eloud-eval-advice)
        (advice-remove 'eval-region 'eloud-eval-advice)
        (condition-case err
            (let ((fx (car r))
                  (args (cdr r)))
              (call-interactively fx args)
              (unless args
                ;; NB: We only want this spoken when we are called directly
                ;; by the user, i.e. without arguments.
                (eloud-speak "eval complete" nil t t)))
          ('error
           ;; NB:  We want error messages spoken regardless of how the advisee was called.
           (eloud-speak (format "%S" err) nil t t)
           (error err))))
    (advice-add 'eval-region :around 'eloud-eval-advice)
    (advice-add 'eval-buffer :around 'eloud-eval-advice)))

(defun eloud-eval-activate ()
  (interactive)
  (eloud-command-map-activate eval)
  (message "eloud eval is now ON"))

(defun eloud-eval-deactivate ()
  (interactive)
  (eloud-command-map-deactivate eval)
  (message "eloud eval is now OFF"))

(defun eloud-eval-toggle-activation ()
  "Toggle eloud eval support on and off."
  (if eloud-mode
      (eloud-eval-activate)
    (eloud-eval-deactivate)))


(provide 'eloud-eval)


(define-eloud-command-map eval)
(define-eloud-command-map-advice
  eval
  'eval-buffer
  'eloud-eval-advice
  :around)
(define-eloud-command-map-advice
  eval
  'eval-region
  'eloud-eval-advice
  :around)
(eloud-command-map-deactivate eval)
(add-hook 'eloud-mode-hook 'eloud-eval-toggle-activation)


;; eloud-eval.el ends here.
