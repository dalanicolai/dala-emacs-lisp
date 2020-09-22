;; (add-hook 'pdf-view-mode-hook (lambda () (when (equal pdf-view-continuous 'hack)
;;                                       (setq mode-line-format nil))))

(defun pdf-view-toggle-continuous ()
  (interactive)
  (if (equal pdf-view-continuous t)
      (setq pdf-view-continuous 'hack)
    (setq pdf-view-continuous t)
    (condition-case nil
        (windmove-down)
      (error nil))
    (delete-window)))

(defun pdf-view-next-line-or-next-page (&optional arg)
  "Scroll upward by ARG lines if possible, else go to the next page.

When `pdf-view-continuous' is non-nil, scrolling a line upward
at the bottom edge of the page moves to the next page."
  (interactive "p")
  (cond ((equal pdf-view-continuous 'hack)
         (let ((hscroll (window-hscroll))
               (cur-page (pdf-view-current-page)))
	         (print (format
                   "window-total-height %s, frame-height %s\nnext line: vscroll value, second next line: output value (image-next-line)"
                   (window-total-height)
                   (frame-height))
                   (get-buffer-create "*pdf-scroll-log*"))
           (when (= (print (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll) (get-buffer-create "*pdf-scroll-log*"))
                    (print (image-next-line arg) (get-buffer-create "*pdf-scroll-log*")))
	           (cond
	            ((if (equal (frame-parameter nil 'fullscreen) 'fullboth)
                   (< (window-total-height) (- (frame-height) 1))
                 (< (window-total-height) (frame-height)))
               (condition-case nil
                   (window-resize (get-buffer-window) -1 nil t)
                 (error (delete-window)))
               (image-next-line 1))
              (t
               (display-buffer-in-direction (current-buffer) (cons '(direction . below) '((window-height . 1))))
               (windmove-down)
               (pdf-view-next-page)
               (when (/= cur-page (pdf-view-current-page))
                 (image-bob)
                 (image-bol 1))
               (image-set-window-hscroll hscroll)
               (windmove-up)
               (image-next-line 1))))))
        (pdf-view-continuous 
         (let ((hscroll (window-hscroll))
               (cur-page (pdf-view-current-page)))
           (when (= (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                    (image-next-line arg))
             (pdf-view-next-page)
             (when (/= cur-page (pdf-view-current-page))
               (image-bob)
               (image-bol 1))
             (image-set-window-hscroll hscroll))))
        (t
         (image-next-line 1))))

(defun pdf-view-previous-line-or-previous-page (&optional arg)
  "Scroll downward by ARG lines if possible, else go to the previous page.

When `pdf-view-continuous' is non-nil, scrolling a line downward
at the top edge of the page moves to the previous page."
  (interactive "p")
  (cond ((equal pdf-view-continuous 'hack)
         (let ((hscroll (window-hscroll))
               (cur-page (pdf-view-current-page)))
           (print
            "First line below: vscroll value. Then second line: output value of (image-previous-line)"
            (get-buffer-create "*pdf-scroll-log*"))
           (when (= (print
                     (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                     (get-buffer-create "*pdf-scroll-log*"))
                    (print
                     (image-previous-line arg)
                     (get-buffer-create "*pdf-scroll-log*")))
             (display-buffer-in-direction (current-buffer) (cons '(direction . above) '((window-height . 1))))
             (windmove-up)
             (pdf-view-previous-page)
             (when (/= cur-page (pdf-view-current-page))
               (image-eob)
               (image-bol 1))
             (image-set-window-hscroll hscroll)
             (window-resize (get-buffer-window) 1 nil t))
           (cond ((< (window-total-height) (- (frame-height) window-min-height))
                  (condition-case nil
                      (window-resize (get-buffer-window) 1 nil t)
                    (error nil)))
                 ((= (window-total-height) (- (frame-height) window-min-height))
                  (windmove-down)
                  (delete-window)))))
        (pdf-view-continuous
         (let ((hscroll (window-hscroll))
               (cur-page (pdf-view-current-page)))
           (when (= (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                    (image-previous-line arg))
             (pdf-view-previous-page)
             (when (/= cur-page (pdf-view-current-page))
               (image-eob)
               (image-bol 1))
             (image-set-window-hscroll hscroll))))
        (t
         (image-previous-line arg))))
