
;; eloud support for mu4e email client.


(require 'eloud-command-map)
(require 'eloud)


(defun NOPE-tts-hack-mu4e-input-filter (text)
  (let ((lines (split-string text "\n"))
        (new-lines (list)))
    (dolist (line lines)
      (setq new-lines
            (append
             new-lines
             (list
              (substring line
                         (position-if #'(lambda (ch) (equal ch 32)) line))))))
    (mapconcat #'identity new-lines "\n")))

(defun NOPE-tts-hack-mu4e-speak-headers ()
  (interactive)
  (let ((*netcatbuf-input-hook* #'tts-hack-mu4e-input-filter))
    (netcatbuf "192.168.1.94" 7201)))

;; WAS: net-ddavies-mu4e~headers-move
(defun eloud-mu4e~headers-move-advice (&rest r)
  (let* ((old-func (car r))
         (args (cdr r))
         (output (apply old-func args)))
    (cond
     (output
      (let* ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (txt (substring str (seq-position str ?\ ))))
        (eloud-speak txt)))
     output)))

(defun eloud-mu4e-activate ()
  (interactive)
  (eloud-command-map-activate mu4e)
  (message "eloud mu4e is now ON"))

(defun eloud-mu4e-deactivate ()
  (interactive)
  (eloud-command-map-deactivate mu4e)
  (message "eloud mu4e is now OFF"))

(defun eloud-mu4e-toggle-activation ()
  "Toggle eloud mu4e on and off."
  (if eloud-mode
      (eloud-mu4e-activate)
    (eloud-mu4e-deactivate)))


(provide 'eloud-mu4e)


(define-eloud-command-map mu4e)
(define-eloud-command-map-advice
  mu4e
  'mu4e~headers-move
  'eloud-mu4e~headers-move-advice
  :around)
(eloud-command-map-deactivate mu4e)
(add-hook 'eloud-mode-hook 'eloud-mu4e-toggle-activation)


;; eloud-mu4e.el ends here.
