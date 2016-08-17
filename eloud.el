;;; eloud.el --- A lightweight, interactive screen reader

;; Copyright (C) 2016  Patrick Smyth

;; Author: Patrick Smyth <patricksmyth01@gmail.com>
;; Homepage: https://github.com/smythp/eloud
;; Keywords: extensions
;; Package-Requires: ((emacs "24.4"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Eloud is a lightweight, interactive screen reader.  It uses the espeak speech synthesizer as a backend.

;; Installation

;; 1. Install espeak

;; First, install espeak.  On Ubuntu or Debian, use:

;;     sudo apt-get install espeak

;; On OSX, use:

;;     brew install espeak

;; Or find the compiled version at http://espeak.sourceforge.net/download.html

;; 2. Install the package

;; Clone this repo:

;;     cd ~
;;     git clone https://github.com/smythp/eloud.git

;; Add the load path to your .emacs:

;;     (add-to-list 'load-path "~/eloud/")

;; Finally, set the path to espeak by adding this to your .emacs:

;;     (setq eloud-espeak-path "~/eloud/")
;; Quick install

;;     cd ~
;;     git clone https://github.com/smythp/eloud.git
;;     (add-to-list 'load-path "~/eloud/")
;;     (setq eloud-espeak-path "/usr/bin/espeak")


;;; Code:


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


;;; Define functions used internally

(defvar eloud-pre-command-point)


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


(defun eloud-save-point ()
  (setq eloud-pre-command-point (point)))


;;; Hook functions


(defun eloud-speak-buffer ()
  "Read current buffer aloud."
  (if (< (point-max) 120000)
      (eloud-speak (buffer-string))
    (eloud-speak (buffer-substring (point-min) 120000))))


(defun eloud-rest-of-line ()
  (eloud-speak
   (buffer-substring (point) (line-end-position))))


(defun eloud-speak-after-move ()
  "Reads from point saved in variable eloud-save-point to the new point."
  (eloud-speak (buffer-substring eloud-pre-command-point (point))))


(defun eloud-speak-point (&optional offset)
  "Read character at point aloud or character at point modified by optional OFFSET."
      (eloud-speak
       (if offset
	   (eloud-get-char-at-point offset)
	 (eloud-get-char-at-point))
       eloud-speech-rate t "--punct"))


(defun eloud-speak-buffer-name ()
  "Speak current buffer name."
  (eloud-speak (buffer-name)))



(defun eloud-status-info ()
  "Read status info normally on mode line."
  (interactive)
  (eloud-speak
   (concat (buffer-name) " " (symbol-name major-mode))))


(defun eloud-last-message (&optional offset)
  "Reads message aloud after moving NUM lines from end of message buffer."
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
	(progn
	  (goto-char (point-max))
	  (forward-line offset)
	  (eloud-speak (buffer-substring-no-properties (point) (line-end-position)))))))


(defun eloud-dabbrev-expand ()
  "Speak last dabbrev completion."
  (if dabbrev--last-expansion
      (eloud-speak dabbrev--last-expansion)))


(defun eloud-last-kill-ring ()
  (eloud-speak (car kill-ring)))



(defun eloud-last-input ()
  "Read last input character aloud."
  (let ((last (eloud-get-char-at-point -1))
	(second-last (eloud-get-char-at-point -2)))
    (if (and (equal last " ") (not (equal second-last " ")))
	(progn
	  (save-excursion
	    (goto-char (1- (point)))
	    (eloud-speak (current-word))))
      (eloud-speak last
		   eloud-speech-rate t "--punct"))))


(defvar eloud-hook-map '((minibuffer-setup-hook . eloud-speak-buffer)
			 (post-self-insert-hook . eloud-last-input)))

(setq dabbrev--last-expansion " ")
(setq eloud-post-command-hook-map '((next-line . (eloud-rest-of-line))
				    (previous-line . (eloud-rest-of-line))
				    (forward-word . (eloud-speak-after-move))
				    (backward-word . (eloud-speak-after-move))
				    (forward-char . (eloud-speak-point))
				    (backward-char . (eloud-speak-point))
				    (eval-last-sexp . (eloud-last-message 0))
				    (dabbrev-expand . (eloud-dabbrev-expand))
				    (geiser-eval-last-sexp . (eloud-last-message -1))
				    (move-beginning-of-line . (eloud-rest-of-line))
				    (org-beginning-of-line . (eloud-rest-of-line))
				    (dired-next-line . (eloud-rest-of-line))
				    (kill-word . (eloud-last-kill-ring))
				    (backward-kill-word . (eloud-last-kill-ring))
				    (beginning-of-buffer . (eloud-speak-buffer))
				    (minibuffer-complete . (eloud-completion))))


(setq eloud-pre-command-hook-map '((backward-delete-char-untabify . (eloud-speak-point -1))
				   (delete-forward-char . (eloud-speak-point))
				   (ispell-word . (eloud-speak (eloud-get-buffer-string "*Choices*")))
				   (delete-char . (eloud-speak-point))))



(defun eloud-conditional-hook (hook-map &optional unmap)
  (let ((called-function (car (cdr (assoc this-original-command hook-map))))
	(args (cdr (cdr (assoc this-original-command hook-map)))))
    (cond ((and called-function args) (apply called-function args))
	  (called-function (funcall called-function)))))


(defun eloud-post-command-hook ()
  (eloud-conditional-hook eloud-post-command-hook-map))

(defun eloud-pre-command-hook ()
  (eloud-conditional-hook eloud-pre-command-hook-map))


;;; Speech functions


(defun eloud-ispell-command-loop (&rest r)
  "Advice to read the word being corrected during Ispell.  Call original function with args R."
  (let ((old-func (car r))
        (correction-list (car (cdr r)))
        (other-args (cdr r))
        (word-item-num 0)
        (word (car (cdr (cdr (cdr r)))))
        (list-to-read '()))
    (progn
      ;; (print (car correction-list)))))
      (while correction-list
        (progn
          (push (number-to-string word-item-num) list-to-read)
          (push ". " list-to-read)
          (push (pop correction-list) list-to-read)
          (push ". . . " list-to-read)
          ;; (setq correction-list (cdr correction-list))
          (setq word-item-num (1+ word-item-num))))
      (progn
        (setq list-to-read (concat word " . " (apply 'concat (reverse list-to-read))))
        (eloud-speak list-to-read)
        (let ((output (apply old-func other-args)))
          (progn
            (eloud-speak output)
            output))))))


(defun eloud-isearch-insert (&rest r)
  "Advice to read characters inserted into the minibuffer during isearch.  Call original function with args R."
  (let ((old-func (car r))
        (other-args (cdr r))
        (char-arg (cadr r)))
    (progn
      (eloud-speak (string char-arg))
      (apply old-func other-args))))


(defun eloud-isearch-move (&rest r)
  "Advice to read aloud search word during isearch.  If no additional match in buffer, read \"no match\" aloud.  Call original function with args R."
  (interactive)
  (let ((old-func (car r))
        (other-args (cdr r))
        (direction (cadr r)))
    (progn
      (apply old-func other-args)
      (cond (isearch-error (eloud-speak isearch-error))
            (isearch-success (eloud-speak isearch-string))
            (t (eloud-speak "no match"))))))


;;; add functions to hooks


(defun eloud-map-hooks (hook-map &optional unmap)
  (mapcar (lambda (x)
	    (let ((hook (car x))
		  (function-to-bind (cdr x)))
	      (if (not unmap)
		  (add-hook hook function-to-bind)
		(remove-hook hook function-to-bind))))
	    hook-map))


;;; Define mode

(define-minor-mode eloud-mode "Minor mode for reading text aloud." nil " eloud" :global t
  (if eloud-mode
      (progn
	(add-hook 'post-command-hook 'eloud-post-command-hook)
	(add-hook 'pre-command-hook 'eloud-pre-command-hook)
	(add-hook 'pre-command-hook 'eloud-save-point)
	(eloud-map-hooks eloud-hook-map)
        (eloud-speak "eloud on"))
    (progn
      (remove-hook 'post-command-hook 'eloud-post-command-hook)
      (remove-hook 'pre-command-hook 'eloud-pre-command-hook)	      
      (remove-hook 'pre-command-hook 'eloud-save-point)      
      (eloud-map-hooks eloud-hook-map t)
      (eloud-speak "eloud off"))))


(provide 'eloud)


;;; eloud.el ends here

;; Copyright (C) 2016  Patrick Smyth

;; Author: Patrick Smyth <patricksmyth01>

