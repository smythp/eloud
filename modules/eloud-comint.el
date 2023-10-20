
(require 'subr-x)
(require 'comint)
(require 'eloud-command-map)
(require 'eloud)


;; rm: remove write-protected regular empty file 'x'? 
(defcustom eloud-comint-rm-ro-prompt-regexp
  "rm: remove write-protected [^?]+\\? "
  "Regexp matching prompts for overriding permissions when deleting a file."
  :type 'regexp
  :group 'eloud)

(defvar eloud-comint-watch-for-password-state :dormant)
(defvar eloud-comint-watch-for-password-result-string ""
  "This is used so that speaking responses from the shell
subprocess (e.g. sudo) will not be interrupted or cutoff
entirely by eloud speaking the next prompt.  The TTS mixer interrupts
whatever is being spoken when it receives a new request, so
instead of speaking parts of the response separately we buffer the respons and prepend it to
the prompt text which are then spoken together in a single
request.")

(defun eloud-comint-watch-for-prompts-processor (chunk r)
  (let ((norm-chk (string-trim chunk)))
    (when (> (length norm-chk) 0)
      (cond
       ;; Watch for prompt to override deletion of protected file from rm, et. al.
       ((let ((case-fold-search t))
          (string-match eloud-comint-rm-ro-prompt-regexp chunk))
        (eloud-speak norm-chk nil t t))
       ;; We look to see if we have a password prompt.  If we do then we
       ;; start speaking until we see a regular old command line prompt,
       ;; at which time we stop speaking.  In between we speak the output,
       ;; which is the negotiation for a correct password before or until
       ;; the number of tries is exceeded.
       ((let ((case-fold-search t))
          (string-match comint-password-prompt-regexp chunk))
        (setq eloud-comint-watch-for-password-state :speaking)
        (let ((otxt (concat eloud-comint-watch-for-password-result-string "  " (string-trim (match-string 0 chunk)))))
          (eloud-speak otxt nil t t)
          (setq eloud-comint-watch-for-password-result-string "")))
       ((let ((case-fold-search t))
          (string-match comint-prompt-regexp chunk))
        (when (> (length eloud-comint-watch-for-password-result-string) 0)
          (eloud-speak eloud-comint-watch-for-password-result-string nil t t)
          (setq eloud-comint-watch-for-password-result-string ""))
        (setq eloud-comint-watch-for-password-state :dormant))
       ((equal eloud-comint-watch-for-password-state :speaking)
        ;; NB: It might be tempting to keep adding output to the
        ;; result string, but we only want the /last/ line of output
        ;; in this case, so we do not concatenate.  If we did
        ;; concatenate, then we'd hear all the output of the command
        ;; that is run as super user.  This assumes that a wrong
        ;; password response from sudo has all the info to inform the
        ;; user to try again on a single line (the last one), but
        ;; really that should be the case in any sane sudo
        ;; implementation, so we rely on it.  Also, we must break the
        ;; chunk up into lines and toss all but the last since multiple
        ;; lines, indeed all lines, are often included in the chunk.
       
        (setq eloud-comint-watch-for-password-result-string (car (last (split-string norm-chk "\n"))))))
      (apply r))))

(defun eloud-comint-watch-for-prompts-advice (&rest r)
  (advice-remove 'comint-watch-for-password-prompt 'eloud-comint-watch-for-prompts-advice)
  (eloud-comint-watch-for-prompts-processor (cadr r) r)
  (advice-add 'comint-watch-for-password-prompt :around 'eloud-comint-watch-for-prompts-advice))

(defun eloud-comint-activate ()
  (interactive)
  (eloud-command-map-activate comint)
  (message "eloud comint is now ON"))

(defun eloud-comint-deactivate ()
  (interactive)
  (eloud-command-map-deactivate comint)
  (message "eloud comint is now OFF"))

(defun eloud-comint-toggle-activation ()
  "Toggle eloud comint support on and off.  This is installed as a normal hook
on the ELOUD-MODE-HOOK minor mode hook variable."
  (if eloud-mode
      (eloud-comint-activate)
    (eloud-comint-deactivate)))

(provide 'eloud-comint)

(define-eloud-command-map comint)
(define-eloud-command-map-advice
  comint
  'comint-watch-for-password-prompt
  'eloud-comint-watch-for-prompts-advice
  :around)
(eloud-command-map-deactivate comint)
(add-hook 'eloud-mode-hook 'eloud-comint-toggle-activation)


;; eloud-comint.el ends here
