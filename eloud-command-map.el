;; ;; NOTE: Before eloud-command-map can manage function advice and hook functions,
;; it is best for the advised functions and hook definitions to be loaded.
;; Therefore eloud.el should probably be loaded toward the end of the user's .emacs file so that
;; yet to be made definitions for advised functions do not cause warnings at eloud
;; ;; load time and so that typos in hook/advice maps can be reported..

;; Managment of hooks and adviced functions.  Provides definition and
;; undefinition of groups of hook functions and/or advise functions
;; that can be dynamically activated and deactivated.  For instance a
;; set of hook and advice functions may be defined as a global group
;; which cand be activated at emacs start-up time when eloud is
;; loaded.  Other groups can be defined and activated via e.g., hook
;; functions (standard or eloud managed), by advice added by the
;; globals or through user-defined functions bound to keys, etc..

(defvar eloud-command-map-warning-type '(eloud command-map))

(defun eloud-cm-make-q-map-name (map-name qualifier)
  ;;(assert (or (eq qualifier :hook) (eq qualifier :advice)))
  (intern (format "$eloud-%s-%s-map"
                  (symbol-name map-name)
                  (intern (substring (symbol-name qualifier) 1)))))

;; NB: ADVICE-TYPE is not a parameter, since emacs doesn't allow the
;; same function to advise a function in multiple ways.
;; This is sensible :)
(defun eloud-cm-find-advice (map advisee advisor)
  (let (ret)
    (dolist (ent map)
      (let ((visee (car ent))
            (visor (cadr ent)))
        (when (and (eq advisee visee) (eq advisor visor))
         (push ent ret))))
    ret))

(defun eloud-cm-find-hook (map hook function)
  (let ((foundp nil)
        (p map))
    (while (and p (not foundp))
      (let* ((ent (car p))
             (h-nm (car ent))
             (f-nm (cadr ent)))
        (cond
         ((and (eq h-nm hook)
           (eq f-nm function))
          (setq foundp t))
         (t
          (setq p (cdr map))))))
    (when foundp
      (car p))))

(defun eloud-cm-validate-hook (hook)
  (when (not (symbolp hook))
    (display-warning eloud-command-map-warning-type "eloud can not use non-symbol %S as hook, ignoring." hook)))

(defun eloud-cm-validate-function (fxn)
  (when (not (functionp fxn))
    (display-warning eloud-command-map-warning-type "eloud can not advise non-existant function, %S, ignoring." fxn)))

(defun eloud-cm-activate-hook (hook hook-function depth local)
  (add-hook hook hook-function depth local))

(defun eloud-cm-deactivate-hook (hook hook-function local)
  (remove-hook hook hook-function local))

(defun eloud-cm-activate-advice (advisee advisor advice-type)
  (advice-add advisee advice-type advisor))

(defun eloud-cm-deactivate-advice (advisee advisor advice-type)
  (advice-remove advisee advisor))

(defvar eloud-command-map-map-names '()
  "List of currently defined map names (symbols).")
;;(setq eloud-command-map-map-names '())

(defmacro define-eloud-command-map (map-name)
  "Initialize a new, empty, 'command map', referenced by
MAP-NAME, an unevaluated symbol.

A command map abstracts both hook definitions and advice
definitions and manages hook and advice 'activation'.  It
provides a store for hook and advice definitions, a namespace and
dynamic scoping for installing and removing hook functions as well as
for adding and removing function advice."
  `(cond
    ((member ',map-name eloud-command-map-map-names)
     (error "eloud command map %S is already defined." ',map-name))
    (t
     (message "Defining new eloud command map '%S', existing command maps are: %S ." ',map-name eloud-command-map-map-names)
     (push ',map-name eloud-command-map-map-names)
     ;; Internally, we maintain separate maps for hooks and advice, while the API
     ;; does it's best to keep from exposing that to the user.
     (defvar ,(eloud-cm-make-q-map-name map-name :hook) '())
     (defvar ,(eloud-cm-make-q-map-name map-name :advice) '()))))

(defmacro undefine-eloud-command-map (map-name)
  "Remove all side effects resulting from definition of MAP-NAME."
  `(cond
    ((not (member ',map-name eloud-command-map-map-names))
     (error "eloud map name %S is not defined, so can not undefine it." ',map-name))
    (t
     ;; FIXME: some kind of expansion problme with this!
     ;;(eloud-command-map-deactivate ',map-name)
     (setq eloud-command-map-map-names
           (delete ',map-name eloud-command-map-map-names))
     (makunbound ',(eloud-cm-make-q-map-name map-name :hook))
     (makunbound ',(eloud-cm-make-q-map-name map-name :advice)))))

(defmacro eloud-command-map-length (map-name)
  `(+ (length ,(eloud-cm-make-q-map-name map-name :advice))
      (length ,(eloud-cm-make-q-map-name map-name :hook))))

(defmacro define-eloud-command-map-advice (map-name advisee advisor advice-type)
  ;; NB: We wait to error check until runtime, but we can almost certainly do this at read time.
  `(progn
     ;;(eloud-cm-validate-function ,advisee)
     ;;(eloud-cm-validate-function ,advisor")
     (eloud-cm-activate-advice ,advisee ,advisor ,advice-type)
     (setq ,(eloud-cm-make-q-map-name map-name :advice)
           (push (list ,advisee ,advisor ,advice-type) ,(eloud-cm-make-q-map-name map-name :advice)))))

(defmacro undefine-eloud-command-map-advice (map-name advisee advisor &optional advice-type)
  `(progn
     (eloud-cm-validate-function ',advisee)
     (eloud-cm-validate-function ',advisor)
     (let ((ents (eloud-cm-find-advice ,(eloud-cm-make-q-map-name map-name :advice) ,advisee ,advisor)))
       (cond
        (ents
         (unwind-protect
             (dolist (ent ents)
               (setq ,(eloud-cm-make-q-map-name map-name :advice)
                     (delete ent ,(eloud-cm-make-q-map-name map-name :advice)))
               (eloud-cm-deactivate-advice ,advisee ,advisor (or ,advice-type :around)))))
        (t (display-warning eloud-command-map-warning-type "eloud did not find advice in map %S, ignoring." ',map-name))))))

(defmacro define-eloud-command-map-hook (map-name hook hook-function &optional depth local)
  `(progn
     ;;(eloud-cm-validate-hook ',hook)
     ;;(eloud-cm-validate-function ',hook-function)
     ;; NB:  We don't check for hook-function already on hook (using EQ), because
     ;; ADD-HOOK silently ignores adding duplicates and we already check our own map anyway.
     (push (list ,hook ,hook-function ,depth ,local) ,(eloud-cm-make-q-map-name map-name :hook))
     (eloud-cm-activate-hook ,hook ,hook-function ,depth ,local)))

(defmacro undefine-eloud-command-map-hook (map-name hook-var hook-function &optional local)
  `(progn
     (eloud-cm-validate-hook ',hook-var)
     (eloud-cm-validate-function ',hook-function)
     (let ((ent (eloud-cm-find-hook ,(eloud-cm-make-q-map-name map-name :hook) ,hook-var ,hook-function)))
       (cond
        (ent
         (unwind-protect
             (setq ,(eloud-cm-make-q-map-name map-name :hook)
                   (delete ent ,(eloud-cm-make-q-map-name map-name :hook)))
             (eloud-cm-deactivate-hook ,hook-var ,hook-function ,local)))
        (t
         (warn "eloud: not found in ,map-name %S." ',map-name))))))

(defmacro eloud-command-map-activate (map-name)
  `(progn
     (message "eloud activating command map '%S'" ',map-name)
     (mapcar #'(lambda (x)
                 (eloud-cm-activate-hook (car x) (cdr x) nil nil))
             ,(eloud-cm-make-q-map-name map-name :hook))
     (mapcar #'(lambda (x)
                 (eloud-cm-activate-advice (car x) (cadr x) :around))
             ,(eloud-cm-make-q-map-name map-name :advice))))

(defmacro eloud-command-map-deactivate (map-name)
  `(progn
     (message "eloud deactivating command map '%S'" ',map-name)
     (mapcar (lambda (x)
               (cond
                  ((= (length x) 3)
                   (eloud-cm-deactivate-advice (car x) (cadr x) (caddr x)))
                  (t
                   (error "BUG:  Assertion that advice map entry is of length 3 failed, got %S" x))))
             ,(eloud-cm-make-q-map-name map-name :advice))
     (mapcar #'(lambda (x)
                 (eloud-cm-deactivate-hook (car x) (cdr x) nil))
             ,(eloud-cm-make-q-map-name map-name :hook))))

(provide 'eloud-command-map)

;; eloud-command-map.el ends here

