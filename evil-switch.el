;; Vim's switch plugin equivalent for Emacs (answer to question by Emilien Mottet 30/09/2020)
;; Thanks to
;; Dan @ https://stackoverflow.com/questions/25188206/how-do-you-write-an-emacs-lisp-function-to-replace-a-word-at-point/25188590
(defun evil-switch (pair-alist)
  "Convert pattern at point (or selected region) to predefined pattern."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (pair   (cond ((assoc text pair-alist))
                       ((rassoc text pair-alist)))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (if (equal text (car pair))
          (insert (cdr pair))
        (insert (car pair))))))

(setq pairs-alist '(("true" . "false") ("c" . "d")))

(defun toggle-pairwise-replace ()
  (interactive)
  (evil-switch pairs-alist))

(evil-define-key '(normal visual) 'evil-normal-state-map (kbd "g s") 'toggle-pairwise-replace)
