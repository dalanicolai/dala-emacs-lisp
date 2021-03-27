(require 'google-translate)
(eval-when-compile
  (require 'pdf-view))

(defun pdf-view-current-page (&optional window)
  ;;TODO: write documentation!
  (image-mode-window-get 'page window))

(defun pdf-view-current-image (&optional window)
  ;;TODO: write documentation!
  (image-mode-window-get 'image window))

(defun pdf-links-read-word ()
  "Using PROMPT, interactively read a link-action.
BORROWED FROM `pdf-links-read-link-action'.
See `pdf-links-action-perform' for the interface."
  (pdf-util-assert-pdf-window)
  (let* ((links (pdf-info-textregions
                 (pdf-view-current-page)
                 (current-buffer)))
         (keys (pdf-links-read-link-action--create-keys
                (length links)))
         (key-strings (mapcar (apply-partially 'apply 'string)
                              keys))
         (prompt "Select region (label is printed after region): ")
         (alist (cl-mapcar 'cons keys links))
         (size (pdf-view-image-size))
         (colors (pdf-util-face-colors
                  'pdf-links-read-link pdf-view-dark-minor-mode))
         (args (list
                :foreground (car colors)
                :background "blue"
                :formats
                 `((?c . ,(lambda (_edges) (pop key-strings)))
                   (?P . ,(number-to-string
                           (max 1 (* (cdr size)
                                     pdf-links-convert-pointsize-scale)))))
                 :commands pdf-links-read-link-convert-commands
                 :apply (pdf-util-scale-relative-to-pixel
                         links))))
                         ;; (mapcar (lambda (l) (car (cdr (assq 'edges l))))
                         ;;         links)))))
    (print colors)

    (unless links
      (error "No links on this page"))
    (unwind-protect
        (let ((image-data nil))
          (unless image-data
            (setq image-data (apply 'pdf-util-convert-page args ))
            (pdf-cache-put-image
             (pdf-view-current-page)
             (car size) image-data 'pdf-links-read-link-action))
          (pdf-view-display-image
           (create-image image-data (pdf-view-image-type) t))
          (pdf-links-read-link-action--read-chars prompt alist))
      (pdf-view-redisplay))))

(defun google-translate-pdf-text-region ()
  (interactive)
  (let* ((langs (google-translate-read-args nil nil))
         (source-language (car langs))
         (target-language (cadr langs))
         (bounds nil))
    (google-translate-translate
     source-language target-language
     (pdf-info-gettext (pdf-view-current-page) (pdf-links-read-word)))))

(spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "g" 'google-translate-pdf-text-region)
