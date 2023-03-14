;;; multi-file-pkg.el --- Quickly create multi-file packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@2a02-a45d-af56-1-666c-72af-583a-b92d.fixed6.kpn.net>
;; Keywords: convenience, lisp, tools, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'lisp-mnt)

(defcustom emacs-package-dir (lambda () (file-name-directory buffer-file-name))
  "Directory with versioned package directories for tarring.")

(defun multi-file-create-pkg-file (dir)
  (let* ((base-name (file-name-base buffer-file-name))
	 (full-name (package-desc-full-name (package-buffer-info))) ;versioned-name
	 (version (or (lm-header "package-version") (lm-header "version")))
	 (summary (or (package-desc-summary (package-buffer-info))
		      (error "Package lacks summary")))
	 (deps (read (lm-header-multiline "package-requires"))))
    (with-temp-file (concat dir
			    (file-name-as-directory full-name)
			    base-name "-pkg.el")
      (prin1 (append (list 'define-package
			   base-name
			   version
			   summary
			   (when (or deps multi-file-deps)
			     deps)))
	     (current-buffer)))))

(defun multi-file-pkg-create ()
  "Call from main package file."
  (interactive)
  (setq emacs-package-dir (if (functionp emacs-package-dir)
			      (funcall emacs-package-dir)
			    emacs-package-dir))
  (let* ((full-name (package-desc-full-name (package-buffer-info)))
	 (files (read (lm-header "package-files")))
	 (package-dir (file-name-as-directory (concat emacs-package-dir full-name))))
    (make-directory package-dir t)
    (dolist (f files)
      (copy-file f (concat package-dir f)))
    (multi-file-create-pkg-file emacs-package-dir)
    (let ((default-directory emacs-package-dir))
      (call-process "tar" nil nil nil "-cf" (concat full-name ".tar") full-name))))

(provide 'multi-file-pkg)
;;; multi-file-pkg.el ends here
