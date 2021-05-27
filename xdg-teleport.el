;; (require 'mimetypes nil t)

(defvar all-the-icons-app-icon-alist
  '(
    ("Other"                    all-the-icons-faicon "rocket")

    ("Chromium Web Browser"     all-the-icons-faicon "chrome"       :height 1.0 :face all-the-icons-lblue)
    ("Google Chrome (unstable)" all-the-icons-faicon "chrome"                   :face all-the-icons-lblue)
    ("Firefox on X11"           all-the-icons-faicon "firefox"                  :face all-the-icons-orange)
    ("Firefox"                  all-the-icons-faicon "firefox"                  :face all-the-icons-orange)
    ("Web"                      all-the-icons-faicon "globe"        :height 1.0 :face all-the-icons-blue)
    ("qutebrowser"              all-the-icons-faicon "globe"        :height 1.0 :face all-the-icons-blue)

    ("Emacs"                    all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.1 :face all-the-icons-purple)
    ))

;;;###autoload
(defun all-the-icons-icon-for-app (app &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (or (cdr (assoc app
                            all-the-icons-app-icon-alist))
                   (cdr (assoc "Other"
                               all-the-icons-app-icon-alist))))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

(defvar freedesktop-data-directories (subseq (split-string
                                              (shell-command-to-string
                                               "$XDG_DATA_DIRS") ":") 2 -1))


(defun freedesktop-list-mimetypes ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents-literally "/etc/mime.types")
    (re-search-forward "# MIME")
    (let (mime-list)
      (while (not (eobp))
        (forward-line)
        (push (car (split-string (thing-at-point 'line t))) mime-list))
      (if (called-interactively-p 'any)
          (completing-read "Select mime-type: " (nreverse mime-list))
        mime-list))))

;; (mapcar (lambda (x) (file-exists-p (concat (string-trim x) "/applications/mimeinfo.cache"))) freedesktop-data-directories)

;; (defun freedesktop-get-mime-app-desktop-files (mime-type)
;;   (let (ids-list)
;;     (mapcar (lambda (x) 
;;               (when (file-exists-p (concat (string-trim x) "/applications/mimeinfo.cache"))
;;                 (with-temp-buffer
;;                   (insert-file-contents-literally (concat (string-trim x) "/applications/mimeinfo.cache"))
;;                   (while (re-search-forward (concat "^" mime-type) nil t)
;;                     (setq ids-list (append ids-list
;;                                            (split-string
;;                                             (string-trim-right
;;                                              (cadr (split-string (string-trim-right (thing-at-point 'line t) "\n") "="))
;;                                              ";")
;;                                             ";")))))))
;;             freedesktop-data-directories)
;;     ids-list))

(defun freedesktop-get-file-mime-type (file)
  (interactive (list (url-filename (url-generic-parse-url buffer-file-name))))
  (let ((mime-type (string-trim-right
                    (shell-command-to-string (format "xdg-mime query filetype '%s'" file))
                    "\n")))
    (if (called-interactively-p 'any)
        (print mime-type)
      mime-type)))

(defun freedesktop-get-mime-app-desktop-files (mime-type)
  (interactive (list (call-interactively 'freedesktop-get-file-mime-type)))
  (with-temp-buffer
    (insert-file-contents-literally "/usr/share/applications/mimeinfo.cache")
    (let (ids-list)
      (while (re-search-forward (concat "^" mime-type) nil t)
        (setq ids-list (append ids-list
                               (split-string
                                (string-trim-right
                                (cadr (split-string (string-trim-right (thing-at-point 'line t) "\n") "="))
                                ";")
                                ";"))))
      (if (called-interactively-p 'any)
          (completing-read "Select desktop file: " ids-list)
        ids-list))))

(defun freedesktop-get-desktop-file (&optional arg desktop-file)
  (interactive "P")
   (let ((desktop-file (if arg
                           (call-interactively 'freedesktop-get-mime-app-desktop-files)
                         desktop-file))
         (default-directory "/usr/share/applications/"))
    (if arg
        (find-file desktop-file)
      (counsel-find-file))))


(defun freedesktop-get-app-data (app-id)
  (let ((default-directory "/usr/share/applications/"))
    (with-temp-buffer
      (insert-file-contents-literally app-id)
      (mapcar (lambda (x)
                (goto-char (point))
                (when (re-search-forward x nil t)
                  (substring (cadr (split-string (thing-at-point 'line t) "="))
                                  0 -1)))
              ;; '("^Name" "^Exec" "^Icon")))))
              '("^Name" "^Exec")))))

(defun freedesktop-open-with (file &optional arg)
  (interactive (list (url-filename (url-generic-parse-url buffer-file-name)) current-prefix-arg))
  (let* ((default-directory "/usr/share/applications/")
         (mime (string-trim-right
                (shell-command-to-string (format "xdg-mime query filetype '%s'" (expand-file-name file)))
                "\n"))
                ;; (mimetypes-guess-mime (expand-file-name file))))
         (apps (mapcar (lambda (x) (freedesktop-get-app-data x))
                       (freedesktop-get-mime-app-desktop-files mime)))
         (app (completing-read "Open file with: "
                        apps))
         (command (car (split-string (car (alist-get app
                                                     apps nil nil 'equal))))))
    (if arg
        (print app)
      (call-process command nil 0 nil file))))
      ;; (start-process app nil "nohup" command file))))

(defun embark-open-with (file)
  (freedesktop-open-with file))

(ivy-add-actions
 t
 '(("a" freedesktop-open-with "open-with")))
