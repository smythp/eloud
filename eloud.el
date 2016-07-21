(defcustom eloud-speech-rate 270 "Integer from 1 to 400. Sets speech rate for espeak.")
(defcustom eloud-espeak-path "/usr/bin/espeak" "Path to espeak.")


(require 'cl)

(defun hyphen-start-p (string)
  (equal (byte-to-string (aref string 0)) "-"))

(defun eloud-speak (string &optional speed no-kill &rest args)
  "Take a string and pass it to the espeak asynchronous process. Uses the eloud-speech-rate variable if no optional integer speed is specified. Pass additional arguments to espeak as rest arguments. If kill argument non-nil, running speech processes are killed before starting new speech process."
  ;; Defines a function that runs process on list of arguments.
  ;; Defines sensible defaults.
  ;; Run with defaults if no additional args specified in function call, else append additional arguments and run
  (cl-flet ((speak (full-args-list)
		(apply 'start-process full-args-list)))
    (let ((default-args `("eloud-speaking" nil ,eloud-espeak-path ,(if (hyphen-start-p string) (concat " " string) string) "-s" ,(if speed (number-to-string speed) (number-to-string eloud-speech-rate)))))
      (progn
	(if (not no-kill)
	    (progn
	      (start-process "kill-espeak" nil "killall" "espeak")
	      (sleep-for .5)))
	(if (not (equal string ""))
	    (speak (if (not args)
		       default-args
		     (append default-args args))))))))
	

;;;;
;; Speech functions
;;;;


(defun eloud-rest-of-line (&rest r)
  (interactive "^p")
  (let ((move-number (cadr r))
  	(old-func (car r))
  	(additional-args (cdr r)))
    (progn
      (apply old-func additional-args)
      (let ((point-to-end-of-line (buffer-substring (point) (line-end-position))))
	(if (not (equal point-to-end-of-line ""))
	    (eloud-speak point-to-end-of-line))))))


(defun eloud-rest-of-line-delay (&rest r)
  (interactive "^p")
  (let ((move-number (cadr r))
  	(old-func (car r))
  	(additional-args (cdr r)))
    (progn
      (apply old-func additional-args)
      (sit-for .5)
      (let ((point-to-end-of-line (buffer-substring (point) (line-end-position))))
	(if (not (equal point-to-end-of-line ""))
	    (eloud-speak point-to-end-of-line))))))


(defun eloud-whole-buffer (&rest r)
  "Speak whole buffer"
  (interactive "^P")
  (let ((old-func (car r))
  	(n (cadr r)))
    (progn
      (funcall  old-func n)
      (eloud-speak
       (buffer-substring (point-min) (point-max))))))


(defun eloud-status-info ()
  "Read status info normally on mode line."
  (interactive)
  (eloud-speak
   (concat (buffer-name) " " (symbol-name major-mode))))


(defun eloud-current-buffer (&rest r)
  "Read current buffer aloud. Used as advice when switching windows."
  (interactive "^p")
  (let ((old-func (car r))
	(other-args (cdr r)))
  (if r
      (apply old-func other-args))
  (progn
    (eloud-speak (concat (buffer-name) (buffer-substring (point) (point-max))))
    (buffer-name))))


(defun eloud-switch-to-buffer (&rest r)
  "Read current buffer aloud. Used as advice when switching windows."
  (let ((old-func (car r))
	(other-args (cdr r)))
  (if r
      (apply old-func other-args))
  (progn
    (eloud-speak (buffer-name))
    (buffer-name))))

	       
(defun eloud-character-at-point (&rest r)
  "Read aloud the character at point."
  (interactive "^p")
  (let ((old-func (car r))
	(n (cadr r)))
    (progn
      (funcall old-func n)
      (eloud-speak
       (buffer-substring (point) (1+ (point)))
       nil t "--punct"))))


(defun eloud-last-character (&rest r)
  (interactive "^p")
  (let* ((old-func (car r))
	(n (cadr r))
	(other-args (cddr r))
	(cmd (this-command-keys))
	(last-char-cmd (byte-to-string (car (last (string-to-list cmd))))))
    (progn 
      (funcall old-func n)
      (let ((word (save-excursion (search-backward " " (line-beginning-position) t 2))))
	(if (equal cmd " ")
	    (eloud-speak
	     (buffer-substring (point) (if word word (line-beginning-position)))))
	(eloud-speak
	 (if (> n 1)
	     (concat (number-to-string n) " times " last-char-cmd)
	   last-char-cmd)
	 eloud-speech-rate t "--punct")))))


(defun eloud-last-kill-ring (&rest r)
  "Read last item on killring aloud. To be used as advice."
  (interactive "^p")
  (let* ((old-func (car r))
	 (n (cadr r)))
    (progn
      (funcall old-func n)
      (eloud-speak (car kill-ring)))))


(defun eloud-moved-point (&rest r)
  "After point is moved, read the difference between new point and old point. Used to advise functions."
  (interactive "^p")
  (let ((move-number (cadr r))
	(old-func (car r))
	(additional-args (cddr r))
	(start-point (point)))
    (progn
      (funcall old-func move-number)
      (save-excursion
    	(progn
    	  (eloud-speak (buffer-substring start-point (point))))))))


(defun eloud-evaluation (&rest r)
  "Rads the output of an interactive command aloud when used as advice."
  (interactive "P")
  (let* ((old-func (car r))
	(n (cadr r))
	(other-args (cdr r))
	(output (apply old-func other-args)))
      (eloud-speak (prin1-to-string output))))

			
;;;;
;; Map speech functions to Emacs commands
;;;;


(defvar around-map '((move-beginning-of-line . eloud-rest-of-line)
		     (org-beginning-of-line . eloud-rest-of-line)
		     (beginning-of-buffer . eloud-whole-buffer)
		     (dired-next-line . eloud-rest-of-line)
		     (forward-char . eloud-character-at-point)
		     (backward-char . eloud-character-at-point)
		     (dired-previous-line . eloud-rest-of-line)
		     (next-line . eloud-rest-of-line)
		     (previous-line . eloud-rest-of-line)
		     (other-window . eloud-current-buffer)
		     (switch-to-buffer . eloud-switch-to-buffer)
		     (kill-word . eloud-last-kill-ring)
		     (backward-kill-word . eloud-last-kill-ring)
		     (forward-button . eloud-moved-point)
		     (backward-button . eloud-moved-point)
		     (backward-word . eloud-moved-point)
		     (forward-word . eloud-moved-point)
		     (forward-sentence . eloud-moved-point)
		     (eval-last-sexp . eloud-evaluation)
		     (backward-sentence . eloud-moved-point)
;		     (gnus-topic-select-group . eloud-rest-of-line-delay)
		     (read-from-minibuffer . eloud-read-minibuffer-prompt)
		     (self-insert-command . eloud-last-character)))


(defun map-commands-to-speech-functions (advice-map advice-type &optional unmap)
  "Takes list of cons cells mapping movement commands to eloud speech functions. See variable around-map for example. If optional unmap parameter is t, removes all bound advice functions instead."
  (mapcar (lambda (x)
	    (let ((target-function (car x))
		  (speech-function (cdr x)))
	      (if (not unmap)
		  (advice-add target-function advice-type speech-function)
		(advice-remove target-function speech-function))))
	  advice-map))


(defun eloud-read-minibuffer-prompt (&rest r)
  (let* ((old-func (car r))
	(prompt (cadr r))
	(args (cdr r)))
    (progn
      (eloud-speak prompt)
      (let ((output (apply old-func args)))
;	(eloud-speak output)
	output))))


;;;;
;; Define mode
;;;;

(defun eloud-toggle ()
  "Toggles eloud on or off. Hooked on eloud-mode toggle. Use eloud-mode to turn eloud on or off."
  (if eloud-mode
      (progn
	(map-commands-to-speech-functions around-map :around)
	(eloud-speak "eloud on"))
    (progn
      (map-commands-to-speech-functions around-map nil t)
      (eloud-speak "eloud off"))))


(define-minor-mode eloud-mode "Minor mode for reading text aloud." nil " eloud" :global t)


(add-hook 'eloud-mode-hook 'eloud-toggle)


(provide 'eloud)
