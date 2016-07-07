(setq eloud-speech-rate 270)

(defun eloud-speak (string &optional speed)
  "Take a string and pass it to the espeak asynchronous process. Uses the eloud-speech-rate variable if no speed is specified."
  (flet ((speak (string speed)
	(start-process "eloud-speaking" nil "espeak" "-s" (number-to-string speed) string)))
    (if speed
	(speak string speed)
      (speak string eloud-speech-rate))))


;;;;
;; Speech functions
;;;;

(defun eloud-rest-of-line ()
  "Speak remainder of line aloud."
  (interactive)
  (eloud-speak 
   (buffer-substring (point) (line-end-position))))

(defun eloud-whole-buffer ()
  "Speak whole buffer"
  (interactive)
  (eloud-speak
   (buffer-substring (point-min) (point-max))))

(defun eloud-status-info ()
  "Read status info normally on mode line."
  (interactive)
  (eloud-speak
   (concat (buffer-name) " " (symbol-name major-mode))))


		      
;;;;
;; Map speech functions to Emacs commands
;;;;


(setq advice-map '((next-line . eloud-rest-of-line)
		   (previous-line . eloud-rest-of-line)
		   (beginning-of-buffer . eloud-whole-buffer)))


(defun map-commands-to-speech-functions (advice-map &optional unmap)
  "Takes list of cons cells mapping movement commands to eloud speech functions. See variable advice-map for example. If optional upmap parameter is t, removes all bound advice functions instead."
  (mapcar (lambda (x)
	    (let ((target-function (car x))
		  (speech-function (cdr x)))
	      (if (not unmap)
		  (advice-add target-function :after speech-function)
		(advice-remove target-function speech-function))))
	  advice-map))

(map-commands-to-speech-functions advice-map)



;; (advice-remove 'next-line #'eloud-rest-of-line)
;; (advice-remove 'previous-line #'eloud-rest-of-line)


;; (advice-add 'previous-line :after #'eloud-rest-of-line)
;; (advice-add 'next-line :after #'eloud-rest-of-line)





