;; This file provides functions for consulting vim's help files. Use `M-x
;; vim-help' to select and jump to some subject. Within the help files use `M-x
;; vim-help-find-tag' (or simply `C-]' or `M-.') when the cursor is on a tag. If
;; you have have installed the `link-hint' package then you can also press `o'
;; to follow the links (without having to first position the cursor on the tag).

(require 'evil)
(require 'link-hint nil t)

(defvar vim-help-dir (file-name-directory
                      (car (directory-files-recursively "/usr/share/vim" "^help.txt$"))))

(defvar vim-tags-alist (mapcar (lambda (line)
                                 (split-string line))
                               (string-lines (with-temp-buffer
                                               (insert-file-contents-literally
                                                (concat vim-help-dir "tags"))
                                               (buffer-string)))))

;;;###autoload
(defun vim-help (arg)
  "Select and open subject in vim help file.
When invoked with a prefix then simple open vim help."
  (interactive "P")
  (let* ((location (unless arg
                     (alist-get (completing-read "Select subject: " (mapcar #'car vim-tags-alist))
                                vim-tags-alist nil nil #'string=))))
    ;;      (subject (completing-read "Select subject: " tags)))
    ;; (shell-command (format "gnome-terminal --command \"vim -c 'help %s'\"" subject))))
    (find-file (concat vim-help-dir (or (car location) "help.txt")))
    (vim-help-mode)
    (unless arg
      (evil-ex-search-full-pattern (concat (substring-no-properties (cadr location) 1 -1) "\\*")
                                   1
                                   'forward))))

(defun vim-help-find-tag ()
  (interactive)
  (let* ((symbol-at-point (thing-at-point 'symbol))
         (tag (if (= (aref symbol-at-point 0) 124)
                  (substring-no-properties symbol-at-point 1 -1)
                symbol-at-point))
         ;;      (subject (completing-read "Select subject: " tags)))
         ;; (shell-command (format "gnome-terminal --command \"vim -c 'help %s'\"" subject))))
         (location (alist-get tag vim-tags-alist nil nil #'string=)))
    (when location
      (find-file (concat "/usr/share/vim/vim82/doc/" (car location)))
      (vim-help-mode))
    (evil-ex-search-full-pattern (if location
                                     (concat (substring-no-properties (cadr location) 1 -1) "\\*")
                                   (concat "*" tag "*"))
                                 1
                                 'forward)))

(when (featurep 'link-hint)
  (defun link-hint--next-vim-help-url (&optional bound)
    (interactive)
    (search-forward-regexp "|[^[:space:]]*|" bound t))

  (defun link-hint--vim-help-url-at-point-p ()
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when (> (length symbol-at-point) 1)
        (alist-get (substring-no-properties symbol-at-point 1 -1) vim-tags-alist nil nil #'string=))))

  (defun link-hint--vim-help-find-tag (location)
    (find-file (concat "/usr/share/vim/vim82/doc/" (car location)))
    (vim-help-mode)
    (evil-ex-search-full-pattern (concat (substring-no-properties (cadr location) 1 -1) "\\*")
                                 1
                                 'forward))

  (link-hint-define-type 'vim-help-url
    :next #'link-hint--next-vim-help-url
    :at-point-p #'link-hint--vim-help-url-at-point-p
    :open #'link-hint--vim-help-find-tag
    :copy #'kill-new)

  (push 'link-hint-vim-help-url link-hint-types))

(define-derived-mode vim-help-mode special-mode "Vim help"
  (setq tab-width 8)
  (modify-syntax-entry ?\" "w"))

(define-key vim-help-mode-map (kbd "M-.") 'vim-help-find-tag)
(when (featurep 'link-hint)
  (define-key vim-help-mode-map "o" 'link-hint-open-link))

(evil-define-key '(motion normal insert) vim-help-mode-map (kbd "C-]") 'vim-help-find-tag)
(evil-define-key '(normal insert) vim-help-mode-map "o" 'link-hint-open-link)

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
