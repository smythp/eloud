(defgroup eloud nil
  "Customization group for the Eloud screen reader package."
  :group 'multimedia)

(defcustom eloud-speech-rate 270
  "Integer from 1 to 400. Sets speech rate for espeak."
  :type '(integer)
  :group 'eloud)

(defcustom eloud-espeak-path "/usr/bin/espeak"
  "Path to espeak as string. On OSX, likely to be /usr/local/bin/espeak instead."
  :type '(string)
  :group 'eloud)


;;; Define free variables to avoid compiler errors

(defvar dabbrev--last-expansion)



;;; Helper functions

(defun eloud-hyphen-start-p (string)
  "Test if first character of STRING is a hyphen."
  (equal (byte-to-string (aref string 0)) "-"))


(defun eloud-get-buffer-string (buffer)
  "Return a string with the contents of BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (buffer-string))))


(defun eloud-get-char-at-point (&optional offset return-edge)
  "Return string of char at point or optional OFFSET.  If optional RETURN-EDGE is non-nil, return character at point min or point max if point with offset exceeds buffer size, else return an empty string."
  (let* ((new-point (if offset
			(+ (point) offset)
		      (point)))
	 (past-max-p (>= new-point (point-max)))
	 (past-min-p (<= new-point (point-min))))
    (string (char-after (cond (past-max-p (1- (point-max)))
                              (past-min-p (point-min))
                              (t new-point))))))


;;; Main speech function

(defun eloud-speak (string &optional speed no-kill &rest args)
  "Pass STRING to the espeak asynchronous process.  Use the `eloud-speech-rate' variable if no optional integer SPEED is specified.  If NO-KILL argument non-nil, running speech processes are killed before starting new speech process.  Pass additional arguments to espeak as rest ARGS."
  ;; Defines a function that runs process on list of arguments.
  ;; Defines sensible defaults.
  ;; Run with defaults if no additional args specified in function call, else append additional arguments and run
  (let* ((string (if (equal string "") " " string))
	 (default-args `("eloud-speaking" nil ,eloud-espeak-path ,(if (eloud-hyphen-start-p string) (concat " " string) string) "-s" ,(if speed (number-to-string speed) (number-to-string eloud-speech-rate)))))
    (if (not (current-idle-time))
	(progn
	  (if (not no-kill)
	      (progn
		(start-process "kill-espeak" nil "killall" "espeak")
		(sleep-for .5)))
	  (if (not (equal string ""))
	      (apply #'start-process (if (not args)
					 default-args
				       (append default-args args))))))))



;;; hook functions


(defun eloud-speak-buffer ()
  "Read current buffer aloud."
  (if (< (point-max) 120000)
      (eloud-speak (buffer-string))
    (eloud-speak (buffer-substring (point-min) 120000))))


(defun eloud-map-hooks (hook-map &optional unmap)

  (mapcar (lambda (x)
	    (let ((hook (car x))
		  (function-to-bind (cdr x)))
	      (if (not unmap)		  
		  (add-hook hook function-to-bind)
		(remove-hook hook function-to-bind))))
	  hook-map))
  

(defvar eloud-hook-map '((minibuffer-setup-hook . eloud-speak-buffer)))





(define-minor-mode eloud-mode "Minor mode for reading text aloud." nil " eloud" :global t
  (if eloud-mode
      (progn
;;        (eloud-map-commands-to-speech-functions eloud-around-map :around)
	;;        (eloud-map-commands-to-hooks eloud-hook-map)
	(eloud-map-hooks eloud-hook-map)	
        (eloud-speak "eloud on"))
    (progn
      ;; (eloud-map-commands-to-speech-functions eloud-around-map nil t)
      ;; (eloud-map-commands-to-hooks eloud-hook-map t)
      (eloud-map-hooks eloud-hook-map t)
      (eloud-speak "eloud off"))))





