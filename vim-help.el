;; This file provides functions for consulting vim's help files. Use `M-x
;; vim-help' to select and jump to some subject. Within the help files use `M-x
;; vim-help-find-tag' (or simply `M-.') when the cursor is on a tag.

(require 'evil)

;;;###autoload
(defun vim-help (arg)
  "Select and open subject in vim help file.
When invoked with a prefix then simple open vim help."
  (interactive "P")
  (let* ((doc-dir (file-name-directory
                   (car (directory-files-recursively "/usr/share/vim" "^help.txt$"))))
         (tags-data (string-lines
                    (with-temp-buffer
                      (insert-file-contents-literally
                       (concat doc-dir "tags"))
                      (buffer-string))))
         (tags-list (mapcar (lambda (line)
                              (split-string line))
                            tags-data))
         (tags (mapcar #'car tags-list))
    ;;      (subject (completing-read "Select subject: " tags)))
    ;; (shell-command (format "gnome-terminal --command \"vim -c 'help %s'\"" subject))))
         (location (unless arg
                     (alist-get (completing-read "Select subject: " tags)
                                tags-list nil nil #'string=))))
    (find-file (concat doc-dir (or (car location) "help.txt")))
    (vim-help-mode)
    (unless arg
      (evil-ex-search-full-pattern (concat (substring-no-properties (cadr location) 1 -1) "\\*")
                                   1
                                   'forward))))

(defun vim-help-find-tag ()
  (interactive)
  (let* ((doc-dir (file-name-directory
                       (car (directory-files-recursively "/usr/share/vim" "^help.txt$"))))
         (symbol-at-point (thing-at-point 'symbol))
         (tag (if (= (aref symbol-at-point 0) 124)
                  (substring-no-properties symbol-at-point 1 -1)
                symbol-at-point))
         (tags-data (string-lines
                     (with-temp-buffer
                       (insert-file-contents-literally
                        (concat doc-dir "tags"))
                       (buffer-string))))
         (tags-list (mapcar (lambda (line)
                              (split-string line))
                            tags-data))
         (tags (mapcar #'car tags-list))
         ;;      (subject (completing-read "Select subject: " tags)))
         ;; (shell-command (format "gnome-terminal --command \"vim -c 'help %s'\"" subject))))
         (location (alist-get tag tags-list nil nil #'string=)))
    (when location
      (find-file (concat "/usr/share/vim/vim82/doc/" (car location)))
      (vim-help-mode))
    (evil-ex-search-full-pattern (if location
                                     (concat (substring-no-properties (cadr location) 1 -1) "\\*")
                                   (concat "*" tag "*"))
                                 1
                                 'forward)))

(define-derived-mode vim-help-mode special-mode "Vim help"
  (setq tab-width 8)
  (modify-syntax-entry ?\" "w"))

(define-key vim-help-mode-map (kbd "M-.") 'vim-help-find-tag)

(evil-define-key '(motion normal insert) vim-help-mode-map (kbd "C-]") 'vim-help-find-tag)

(defun vim-help-font-lock-add-keywords ()
  (font-lock-add-keywords nil '(("[[:space:]^]*\\*[A-z0-9-_:.]*\\*" . font-lock-constant-face)
                                ("^.*~$" . font-lock-builtin-face)
                                ;; ("|[:a-z0-9-_.+]*|" . font-lock-keyword-face)
                                ("|[^[:space:]]*|" . font-lock-keyword-face)
                                ("^[[:space:]][V:].*$" . font-lock-keyword-face)
                                ("^[-=]*$" . font-lock-keyword-face)
                                ("Vim version [0-9]\\.[0-9]" . font-lock-keyword-face)
                                ("<[^[:space:]]*>" . font-lock-doc-face)
                                ("{\\w*}" . font-lock-doc-face)
                                ("\\bCTRL-." . font-lock-doc-face)
                                ("'[a-z]*'" . font-lock-string-face))))

(add-hook 'vim-help-mode-hook #'vim-help-font-lock-add-keywords)
