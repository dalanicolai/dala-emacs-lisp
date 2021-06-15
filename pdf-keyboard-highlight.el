(defcustom pdf-keyboard-annot-default-type "highlight"
  "Default annotation type for keyboard annotation"
  :type '(string)
  :options '("highlight" "squiggly" "strike-out" "underline")
  :group 'pdf-annot)

(defun pdf-keyboard-annot-format-collection (search-results)
  "Transform SEARCH-RESULTS into useful collection.
The collection is given to completing-read in the
`pdf-keyboard-hightlight' function."
  (mapcar (lambda (x)
            (let ((y (cdr x)))
              (cons (cdar y)
                    (cdr y))))
          search-results))

(defun pdf-keyboard-highlight (&optional arg)
  "Create markup annotation using the keyboard. Prompts for start
pattern, i.e. usually (part of) beginning of a word,and end
pattern, usually the (end of) a word, for the text region to
annotate. Creates type of `pdf-keyboard-annot-default-type' by
default. When prefixed with universal argument
\[universal-argument], the command additionally prompts for
selecting an annotation type.

Unfortunately, in some documents the edges (size of the region)
are not translated correctly"
  (interactive "P")
  (let* ((from (pdf-keyboard-annot-format-collection
                (pdf-info-search-string (read-string "From: ")
                                        (pdf-view-current-page))))
         (to (let ((patt (read-string "To: ")))
               (unless (string= patt "")
                 (pdf-keyboard-annot-format-collection
                  (pdf-info-search-string patt
                                          (pdf-view-current-page))))))
         (start-coords (if (= (length from) 1)
                           (cadr (cadar from))
                         (cadar (alist-get
                                 (completing-read "Select correct START context: " from)
                                 from nil nil 'equal))))
         (end-coords (when to
                       (if (= (length to) 1)
                          (cadr (cadar to))
                        (cadar (alist-get
                                (completing-read "Select correct END context: " to)
                                to nil nil 'equal)))))
         (edges (if to
                    (append (cl-subseq start-coords 0 2) (cl-subseq end-coords 2 4))
                  start-coords)))
    (pcase (if arg
               (read-answer "Create annotation of markup type? "
                '(("highlight"  ?h "perform the action")
                  ("squiggly"   ?s "skip to the next")
                  ("strike-out" ?o "accept all remaining without more questions")
                  ("underline"  ?u "accept all remaining without more questions")
                  ("help" ?h "show help")
                  ("quit" ?q "exit")))
             pdf-keyboard-annot-default-type)
      ("highlight"  (pdf-annot-add-highlight-markup-annotation edges))
      ("squiggly"   (pdf-annot-add-squiggly-markup-annotation edges))
      ("strike-out" (pdf-annot-add-strikeout-markup-annotation edges))
      ("underline"  (pdf-annot-add-underline-markup-annotation edges)))))
