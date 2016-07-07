
;  (thing-at-point 'line))

;; (defvar eloud-mode nil
;;   "Mode variable for eloud mode.")

;; (defun eloud-mode (&optional arg)
;;   "Eloud minor mode toggle."
;;   (interactive "P")
;;   (setq eloud-mode
;; 	(if (null arg)
;; 	    (not eloud-mode)
;; 	  (> (prefix-numeric-value arg) 0)))
;;   (if eloud-mode
;;       code for turning on eloud-mode
;;       code for turning off eloud-mode))

;; ; add to mode line
;; (if (not (assq 'eloud-mode minor-mode-alist))
;;     (setq minor-mode-alist
;; 	  (cons '(eloud-mode " eloud")
;; 		minor-mode-alist



;; (thing-at-point 'line)

;; (current-line)

;; (save-excursion
;;   (goto-char (point-min)))




(setq eloud-speech-rate 270)

(defun eloud-speak (string &optional speed)
  "Take a string and pass it to the espeak asynchronous process. Uses the eloud-speech-rate variable if no speed is specified."
  (flet ((speak (string speed)
	(start-process "eloud-speaking" nil "espeak" "-s" (number-to-string speed) string)))
    (if speed
	(speak string speed)
      (speak string eloud-speech-rate))))


(defun eloud-rest-of-line ()
  "Speak remainder of line aloud."
  (interactive)
  (eloud-speak 
   (buffer-substring (point) (line-end-position))))

(setq advice-map '((next-line . eloud-rest-of-line)
		   (previous-line . eloud-rest-of-line)))


(defun map-commands-to-speech-functions (advice-map &optional unmap)
  "Takes list of cons cells mapping movement commands to eloud speech functions. See variable advice-map for example. If optional upmap parameter is t, removes all bound advice functions instead."
  (mapcar (lambda (x)
	    (let ((target-function (car x))
		  (speech-function (cdr x)))
	      (advice-add target-function :after #'eloud-rest-of-line)))
	  advice-map))

(eloud-speak "foo")

(advice-remove 'next-line #'eloud-rest-of-line)
(advice-remove 'previous-line #'eloud-rest-of-line)


;; (advice-add 'previous-line :after #'eloud-rest-of-line)
;; (advice-add 'next-line :after #'eloud-rest-of-line)





