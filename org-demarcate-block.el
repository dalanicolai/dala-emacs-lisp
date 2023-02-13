(transient-define-prefix ob-transient ()
  "Some Emacs magic"
  :info-manual "Quickly create org-mode code blocks."
  ["Arguments (save with `C-x s')"
   (ob-transient-language)
   (ob-transient-results)
   (ob-transient-type)
   (ob-transient-format)
   (ob-transient-handling)
   (ob-transient-session)
   ]
  ["Commands"
   ("i" "insert" ob-transient-insert-block)
   ("I" "info" ob-transient-info)
   ("w" "web-info" ob-transient-web-info)
   ("a" "print args" tutorial-print-args)]
  [("q" "Quit" transient-quit-one)])

;; NOTE The `transient-variable' class gets formatted more nicely (i.e. without
;; parentheses, e.g. see magit-branch). However, it sets, and therefore requires
;; the existence of, a variable
(defclass transient-value (transient-argument) ()
  "Class used for command-line argument that can take a value.")

(cl-defmethod transient-infix-read ((obj transient-value))
  (let ((choices (oref obj choices)))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-infix-value ((obj transient-value))
  "Return ARGUMENT and VALUE as a unit or nil if the latter is nil."
  (and-let* ((value (oref obj value)))
    (let ((arg (oref obj argument)))
      (cl-ecase (oref obj multi-value)
        ((nil)    (concat arg value))
        ((t rest) (cons arg value))
        (repeat   (mapcar (lambda (v) (concat arg v)) value))))))

(cl-defmethod transient-format-value ((obj transient-value))
  (let ((value (oref obj value))
        (choices (oref obj choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice value)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                choices
                (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(transient-define-suffix ob-transient-insert-block (&optional args)
  (interactive (list (transient-args 'ob-transient)))
  (let ((language (transient-arg-value "--language=" args))
        (results (transient-arg-value "--results=" args))
        (type (transient-arg-value "--type=" args))
        (handling (transient-arg-value "--handling=" args))
        (session (transient-arg-value "--session=" args)))
    (insert (format "#+BEGIN_SRC %s" language)
            (if session (concat " :session " session) "")
            (if (or results type handling)
                (concat " :results " (or results "value")
                        (when type (concat " " type))
                        (when handling (concat " " handling)))
              "")
            "\n\n"
            "#+END_SRC"))
  (forward-line -1))


(defun tutorial-print-args (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (print args))

(defun ob-transient-info ()
  (interactive)
  (info (Info-find-node "org" "Results of Evaluation")))

(defun ob-transient-web-info ()
  (interactive)
  (browse-url "https://orgmode.org/manual/Results-of-Evaluation.html"))

(transient-define-infix ob-transient-language ()
  :description "Language"
  :class 'transient-option
  :shortarg "l"
  :argument "--language="
  :choices (delete-dups
	    (append (mapcar #'car org-babel-load-languages)
		    (mapcar (lambda (el) (intern (car el)))
			    org-src-lang-modes))))

(transient-define-infix ob-transient-results ()
  :description "Results header options."
  :class 'transient-value
  :shortarg "r"
  :argument "--results="
  :choices '("value" "output"))

(transient-define-infix ob-transient-type ()
  :description "Type header options."
  :class 'transient-value
  :shortarg "R"
  :argument "--type="
  :choices '("table" "vector" "list" "scalar" "verbatim" "file"))

(transient-define-infix ob-transient-format ()
  :description "Type format options."
  :class 'transient-value
  :shortarg "f"
  :argument "--format="
  :choices '("code" "drawer" "html" "latex" "link" "org" "pp" "raw"))

(transient-define-infix ob-transient-handling ()
  :description "Type handling options."
  :class 'transient-value
  :shortarg "h"
  :argument "--handling="
  :choices '("replace" "silent" "none" "discard" "append" "prepend"))

(transient-define-infix ob-transient-session ()
  :description "Results session options."
  :class 'transient-option
  :shortarg "s"
  :argument "--session="
  :choices '("fun"))

;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "bD" #'ob-transient)
