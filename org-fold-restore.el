;;; org-fold-restore.el --- Auto restore folding state of org buffers  -*- lexical-binding: t; -*-

;; NOTE only stores state if buffer has been modified and saved

(defun org-fold-store-state ()
  (let ((state (org-fold-get-regions)))
    (with-temp-file (concat buffer-file-name ".fold")
      (prin1 state (current-buffer)))))

(defun org-fold-restore-state ()
  (let* ((file-base buffer-file-name)
         (state (with-temp-buffer
                 (insert-file-contents-literally
                  (concat file-base ".fold"))
                 (read (current-buffer)))))
    (org-fold-regions state)
    (goto-char (point-min))))


(add-hook 'org-mode-hook 'org-fold-activate)

(defun org-fold-activate ()
  ;; call the restore function
  (org-fold-restore-state)
  ;; and add a local hook, to save the foldstates
  ;; (only is buffer was saved, see `orgfold-kill-buffer')
  (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t))

(defun org-fold-kill-buffer ()
  ;; don't save folding info for unsaved buffers
  (unless (buffer-modified-p)
    (org-fold-store-state)))
