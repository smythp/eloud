
;; Make character movement pronounce capitalized characters.


(require 'eloud-command-map)
(require 'eloud)


(defcustom eloud-casewise-capitalized-indicator "Cap"
  "String used to indicate capitalized letters.")

(defun eloud-casewise-character-at-point-advice (&rest r)
  "Read aloud the character at point, with capitalized letters pronounced as such..  Call original function with args R."
  (interactive "^p")
  (let ((old-func (car r))
        (n (cadr r)))
    (funcall old-func n)
    (let* ((the-str (buffer-substring (point) (1+ (point))))
           (the-ch (string-to-char the-str))
           (ch-props (get-char-code-property the-ch 'general-category)))
      (when (equal ch-props 'Lu)
        (setq the-str (concat eloud-casewise-capitalized-indicator " " the-str)))
      (eloud-speak the-str nil t "--punct"))))

(defun eloud-casewise-activate ()
  (interactive)
  (undefine-eloud-command-map-advice globals 'forward-char 'eloud-character-at-point :around)
  (undefine-eloud-command-map-advice globals 'backward-char 'eloud-character-at-point :around)
  (eloud-command-map-activate casewise)
  (message "eloud casewise is now ON"))

(defun eloud-casewise-deactivate ()
  (interactive)
  (define-eloud-command-map-advice globals 'forward-char 'eloud-character-at-point :around)
  (define-eloud-command-map-advice globals 'backward-char 'eloud-character-at-point :around)
  (eloud-command-map-deactivate casewise)
  (message "eloud casewise is now OFF"))

(defun eloud-casewise-toggle-activation ()
  "Toggle eloud casewise support on and off."
  (if eloud-mode
      (eloud-casewise-activate)
    (eloud-casewise-deactivate)))


(provide 'eloud-casewise)

(define-eloud-command-map casewise)
(define-eloud-command-map-advice
  casewise
  'forward-char
  'eloud-casewise-character-at-point-advice
  :around)
(define-eloud-command-map-advice
  casewise
  'backward-char
  'eloud-casewise-character-at-point-advice
  :around)
(define-eloud-command-map-advice
  casewise
  'previous-line
  'eloud-casewise-character-at-point-advice
  :around)
(define-eloud-command-map-advice
  casewise
  'next-line
  'eloud-casewise-character-at-point-advice
  :around)
(eloud-command-map-deactivate casewise)
(add-hook 'eloud-mode-hook 'eloud-casewise-toggle-activation)


;; eloud-kill-modifiedbuffer.el ends here.

;; eloud-casewise.el ends here.
