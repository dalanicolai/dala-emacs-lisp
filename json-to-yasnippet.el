"From a json snippet buffer run `M-x json-to-yas'.
Subsequently select yasnippet output directory. If output
directory does not exist yet, then the directory will be created.
You can set the snippet-directory variable to your main snippet
directory to navigate faster to the correct directory."

;; (defvar snippet-directory "~/.emacs.d/private/snippets")

(defun json-to-yas-map-keys (key value)
  (pcase key
    ("prefix" (insert
               (concat "# key: " value "\n")))
    ("body"
     (insert "# --\n")
     (mapcar (lambda (x) (insert x) (insert "\n")) value))))

(defun json-to-yas-map (hash)
  (let ((output-directory (read-directory-name "Select snippets output directory: "
                                               (when (boundp 'snippet-directory)
                                                 (expand-file-name snippet-directory)))))
    (unless (directory-name-p output-directory)
      (make-directory output-directory t))
    (maphash (lambda (key value)
               (with-temp-file (concat (file-name-as-directory output-directory)
                                       (string-replace " " "_"
                                                       (downcase key)))
                 (insert "# -*- mode: snippet -*-\n")
                 (insert "# name: " key "\n")
                 (maphash (lambda (key value) (json-to-yas-map-keys key value)) value)))
             hash)))

(defun json-to-yas ()
  (interactive)
  (goto-char (point-min))
  (json-to-yas-map
   (json-parse-buffer :array-type 'list)))
