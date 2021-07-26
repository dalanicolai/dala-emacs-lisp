(require 'ox-md)

(defcustom org-mkdocs-lang-id-translate-alist '(("python" . "py3")
                                                ("emacs-lisp" . "elisp"))
  "Translation table Emacs source block to Pygments language id")

(org-export-define-derived-backend 'mkdocs 'md
  :translate-alist '((headline . org-mkdocs-headline)
		                 (src-block . org-mkdocs-src-block)
                     ;; (template . my-latex-template)
                     ))

(defun org-mkdocs-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (concat "     " (org-make-tag-string tag-list))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (not (memq style '(atx setext)))
	    (and (eq style 'atx) (> level 6))
	    (and (eq style 'setext) (> level 2)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
		  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
	      (concat (org-md--headline-title style level heading tags)
		            contents))))))

(defun org-mkdocs-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((language (org-element-property :language src-block)))
    (format "```%s
%s
```"
            (if-let (id (alist-get language org-mkdocs-lang-id-translate-alist nil nil #'string=))
                id
              language)
            (org-export-format-code-default src-block info))))

;;;###autoload
(defun org-mkdocs-export-as-md (&optional async subtreep visible-only)
  "Export current buffer as a Markdown buffer."
  (interactive)
  (org-export-to-buffer 'mkdocs "*Org MkDocs Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-mkdocs-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown with YAML front matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'mkdocs filename ".md" plist pub-dir))

;;; provide

(provide 'ox-mkdocs)

;;; ox-mkdocs.el ends here
