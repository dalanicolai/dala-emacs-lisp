(defun ediff-with-default-file ()
  (interactive)
  (let* ((git-directory (expand-file-name "~/git"))
         (elpa-directory (expand-file-name "~/.emacs.d/elpa/27.1/develop"))
         (other-file-path (car
                           (directory-files-recursively
                            (if (string-match-p git-directory buffer-file-name)
                                elpa-directory
                              git-directory)
                            (file-name-nondirectory buffer-file-name)))))
    (ediff buffer-file-name other-file-path)))

;; (spacemacs/set-leader-keys "DD" 'ediff-with-default-file)
