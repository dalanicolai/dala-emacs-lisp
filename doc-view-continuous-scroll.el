(defcustom doc-view-cs-step-size 4
  "Step size in lines (integer) for continuous scrolling"
  :group 'doc-view-continuous-scroll
  :type 'integer)

(defun doc-view-cs-window-dual-p ()
  "Return t if current scroll window status is dual, else nil."
  (or (equal 'upper (alist-get 'doc-view-cs-window-status (window-parameters)))
      (equal 'lower (alist-get 'doc-view-cs-window-status (window-parameters)))))

(defun doc-view-cs-close-window-when-dual ()
  (when (doc-view-cs-window-dual-p)
    (let ((window-status (alist-get 'doc-view-cs-window-status (window-parameters))))
      (save-excursion
        (if (equal window-status 'upper)
            (windmove-down)
          (windmove-up))
        (delete-window)
        (set-window-parameter nil 'doc-view-cs-window-status 'single)))))

;; (defun doc-view-cs-create-djvu-buffers ()
;;   (let ((page (doc-view-current-page))
;;         (file buffer-file-name))
;;     ;; (setq-local imenu-create-index-function #'doc-view-imenu-create-index)
;;     ;; (setq-local imenu-default-goto-function (lambda (title page) (doc-view-goto-page page)))
;;     ;; (djvu-find-file file page nil t)))
;;     (let ((djvu-text-buffer (djvu-find-file file page nil t)))
;;       (setq-local imenu-create-index-function #'doc-view-imenu-create-index)
;;       (setq-local imenu-default-goto-function (lambda (title page) (doc-view-goto-page page)))
;;       djvu-text-buffer)))

(defun doc-view-cs-forward-line (&optional arg)
  "Scroll upward by ARG lines if possible, else go to the next page.
This function is an adapted version of
`doc-view-next-line-or-next-page'. Although the ARG is kept here,
this function generally works best without ARG is 1. To increase
the step size for scrolling use the ARG in
`doc-view-cs-forward'"
  (if doc-view-continuous-scroll-mode
         (let ((hscroll (window-hscroll))
               (cur-page (doc-view-current-page)))
	         (print (format
                   "window-total-height %s, frame-height %s\nnext line: vscroll value, second next line: output value (image-next-line)"
                   (window-total-height)
                   (frame-height))
                  (get-buffer-create "*doc-view-scroll-log*"))
           (when (= (print
                     (window-vscroll nil t)
                     (get-buffer-create "*doc-view-scroll-log*"))
                    (print
                     (image-next-line arg)
                     (get-buffer-create "*doc-view-scroll-log*")))
	           (cond
	            ((not (window-full-height-p))
               (condition-case nil
                   (window-resize (get-buffer-window) -1 nil t)
                 (error (kill-buffer (alist-get 'djvu-text-buffer (window-parameters)))
                        (delete-window)
                        (set-window-parameter nil 'doc-view-cs-window-status 'single)))
               (image-next-line 1))
              (t
               (if (= (doc-view-current-page) (doc-view-last-page-number))
                   (message "No such page: %s" (+ (doc-view-current-page) 1))
                 (display-buffer-in-direction
                  (current-buffer)
                  (cons '(direction . below) '((window-height . 1))))
                 (set-window-parameter nil 'doc-view-cs-window-status 'upper)
                 (windmove-down)
                 (set-window-parameter nil 'doc-view-cs-window-status 'lower)
                 (doc-view-goto-page cur-page)
                 (doc-view-next-page)
                 (when (/= cur-page (doc-view-current-page))
                   (image-bob)
                   (image-bol 1))
                 (image-set-window-hscroll hscroll)
                 (windmove-up)
                 (image-next-line 1))))))
    (message "doc-view-continuous-scroll-mode not activated")))

(defun doc-view-cs-forward (arg)
  (interactive "P")
  (let ((arg (or arg doc-view-cs-step-size)))
    (dotimes (_ arg) (doc-view-cs-forward-line 1))))

(defun doc-view-cs-backward-line (&optional arg)
  "Scroll down by ARG lines if possible, else go to the previous page.
This function is an adapted version of
`doc-view-previous-line-or-previous-page'. Although the ARG is
kept here, this function generally works best without ARG is 1.
To increase the step size for scrolling use the ARG in
`doc-view-cs-backward'"
  (if doc-view-continuous-scroll-mode
      (let ((hscroll (window-hscroll))
            (cur-page (doc-view-current-page)))
        (print
         "First line below: vscroll value. Then second line: output value of (image-previous-line)"
         (get-buffer-create "*pdf-scroll-log*"))
        (when (and (= (print
                       (window-vscroll nil t)
                       (get-buffer-create "*doc-view-scroll-log*"))
                      (print
                       (image-previous-line arg)
                       (get-buffer-create "*doc-view-scroll-log*"))))
          (if (= (doc-view-current-page) 1)
              (message "No such page: 0")
            (display-buffer-in-direction
             (current-buffer)
             (cons '(direction . above) '((window-height . 1))))
            (set-window-parameter nil 'doc-view-cs-window-status 'lower)
            (windmove-up)
            (set-window-parameter nil 'doc-view-cs-window-status 'upper)
            (doc-view-goto-page cur-page)
            (doc-view-previous-page)
            (when (/= cur-page (doc-view-current-page))
              (image-eob)
              (image-bol 1))
            (image-set-window-hscroll hscroll)
            (window-resize (get-buffer-window) 1 nil t)))
        (cond ((< (window-total-height) (- (frame-height) window-min-height))
               (condition-case nil
                   (window-resize (get-buffer-window) 1 nil t)
                 (error nil)))
              ((= (window-total-height) (- (frame-height) window-min-height))
               (set-window-parameter nil 'doc-view-cs-window-status 'single)
               (windmove-down)
               (delete-window))))
    (message "doc-view-continuous-scroll-mode not activated")))

(defun doc-view-cs-backward (arg)
  (interactive "P")
  (let ((arg (or arg doc-view-cs-step-size)))
    (dotimes (_ arg) (doc-view-cs-backward-line 1))))

;; (defun doc-view-imenu-create-index ()
;;   (with-current-buffer (alist-get 'djvu-text-buffer (window-parameters))
;;     (with-current-buffer (djvu-ref bookmarks-buf djvu-doc)
;;       (goto-char (point-max))
;;       (let (alist)
;;         (while (re-search-backward "\"#p*\\([0-9]+\\).*\"" nil t)
;;           (let ((pagenumber (string-to-number (match-string-no-properties 1))))
;;             (re-search-backward "(\"\\(.+\\)\"")
;;             (push (cons (match-string-no-properties 1) pagenumber) alist)))
;;         alist))))


(setq doc-view-continuous-scroll-mode-map (make-sparse-keymap))
(define-key doc-view-continuous-scroll-mode-map  (kbd "C-n") #'doc-view-cs-forward)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "<down>") #'doc-view-continuous-scroll-forward)
;; (define-key doc-view-continuous-scroll-mode-map (kbd "<wheel-down>") #'pdf-cs-mouse-scroll-forward)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "<mouse-5>") #'pdf-cs-mouse-scroll-forward)
(define-key doc-view-continuous-scroll-mode-map  (kbd "C-p") #'doc-view-cs-backward)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "<up>") #'doc-view-continuous-scroll-backward)
;; (define-key doc-view-continuous-scroll-mode-map (kbd "<wheel-up>") #'pdf-cs-mouse-scroll-backward)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "<mouse-4>") #'pdf-cs-mouse-scroll-backward)
;; (define-key doc-view-continuous-scroll-mode-map  "n" #'doc-view-cs-next-page)
;; (define-key doc-view-continuous-scroll-mode-map  "p" #'doc-view-cs-previous-page)
;; ;; (define-key doc-view-continuous-scroll-mode-map  (kbd "M-<") #'pdf-cscroll-view-goto-page)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "M-g g") #'pdf-cscroll-view-goto-page)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "M-g M-g") #'pdf-cscroll-view-goto-page)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "M-<") #'pdf-cscroll-first-page)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "M->") #'pdf-cscroll-last-page)
;; (define-key doc-view-continuous-scroll-mode-map  [remap forward-char] #'pdf-cscroll-image-forward-hscroll)
;; (define-key doc-view-continuous-scroll-mode-map  [remap right-char] #'pdf-cscroll-image-forward-hscroll)
;; (define-key doc-view-continuous-scroll-mode-map  [remap backward-char] #'pdf-cscroll-image-backward-hscroll)
;; (define-key doc-view-continuous-scroll-mode-map  [remap left-char] #'pdf-cscroll-image-backward-hscroll)
;; (define-key doc-view-continuous-scroll-mode-map  "T" #'pdf-cscroll-toggle-mode-line)
;; (define-key doc-view-continuous-scroll-mode-map  "M" #'pdf-cscroll-toggle-narrow-mode-line)
;; (define-key doc-view-continuous-scroll-mode-map  "Q" #'pdf-cscroll-kill-buffer-and-windows)
;; (define-key doc-view-continuous-scroll-mode-map  (kbd "C-c C-a l") #'pdf-cscroll-annot-list-annotations)


(when (boundp 'spacemacs-version)
  (evil-define-minor-mode-key 'evilified 'doc-view-continuous-scroll-mode
    "j" #'doc-view-cs-forward
    ;;   (kbd "<mouse-5>") #'pdf-cs-mouse-scroll-forward
    "k" #'doc-view-cs-backward
    ))
  ;;   (kbd "<mouse-4>") #'pdf-cs-mouse-scroll-backward
  ;;   "J" #'doc-view-cs-next-page
  ;;   "K" #'doc-view-cs-previous-page
  ;;   (kbd "C-j") #'pdf-view-scroll-up-or-next-page
  ;;   (kbd "C-k") #'pdf-view-scroll-down-or-previous-page
  ;;   (kbd "g t") #'pdf-cscroll-view-goto-page
  ;;   (kbd "g g") #'pdf-cscroll-first-page
  ;;   "G" #'pdf-cscroll-last-page
  ;;   "M" #'pdf-cscroll-toggle-mode-line
  ;;   "q" #'pdf-cscroll-kill-buffer-and-windows
  ;;   "l" #'pdf-cscroll-image-forward-hscroll
  ;;   "h" #'pdf-cscroll-image-backward-hscroll)
  ;; (spacemacs/set-leader-keys-for-minor-mode
  ;;   'doc-view-continuous-scroll-mode
  ;;   (kbd "a l") #'pdf-cscroll-annot-list-annotations))

(define-minor-mode doc-view-continuous-scroll-mode
  "Emulate continuous scroll with two synchronized buffers"
  nil
  " Continuous"
  doc-view-continuous-scroll-mode-map
  (unless doc-view-continuous-scroll-mode
    (doc-view-cs-close-window-when-dual))
  (set-window-parameter nil 'doc-view-cs-window-status 'single))
