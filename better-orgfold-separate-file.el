;; improved/cleaned up version of
;; https://www.emacswiki.org/emacs/orgfold-separate-file.el

;; the single improvement is that this version prevents that the created '.fold'
;; files get added to the `recentf-list'. Furthermore some redundant code has
;; been removed. Thanks to Cassiel-girl.

;; NOTE for org-versions >= 9.6 use the org-fold-restore.el file in
;; this repo (https://github.com/dalanicolai/dala-emacs-lisp)

;; Copyright (C) 2009-2022

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;
;; Proof of concept implementation of saving folding information in
;; org buffers. This variant saves folding information in a separate
;; file.
;; 

(defun orgfold-get-fold-info-file-name ()
  (concat (buffer-file-name) ".fold"))

(defun orgfold-save ()
  (save-excursion
    (goto-char (point-min))

    (let (foldstates)
      ;; if not at a headline, then jump to first headline
      (unless (looking-at outline-regexp)
        (outline-next-visible-heading 1))

      ;; collapsing a headline is achieved by hiding the 'contents'
      ;; hiding text is done by adding the 'invisible' text/overalay-property
      ;; so to check if a headline is collapsed, the cursor is placed at the end
      ;; of a headline, and if any of the overlays at that location have the a
      ;; non-nil invisible overlay property, then `t` is pushed (added) to foldstates
      (while (not (eobp)) ; (end of buffer predicate), continue jumping to next
                          ; headline until end of buffer has been reached
        (push (when (seq-some (lambda (o) (overlay-get o 'invisible))
                         (overlays-at (line-end-position)))
                t)
              foldstates)
        ;; jump to next headline or otherwis to end of buffer (eob)
        (outline-next-visible-heading 1))

      ;; despite the name (temp file) this creates a new file, then adds
      ;; contents and saves it (permanently, it that sense 'temp' is a misnomer)
      ;; because push adds to beginning of list, the last headlines are at the
      ;; front of the list, so the 'foldstates' get inserted in reverse
      (with-temp-file (orgfold-get-fold-info-file-name)
        (prin1 (nreverse foldstates) (current-buffer))))))

(defun orgfold-restore ()
  (save-excursion
    (goto-char (point-min))
    (let* ((foldfile (orgfold-get-fold-info-file-name))

           ;; this simply reads the content of the file
           (foldstates
            ;; first check if file exists
            (when (file-readable-p foldfile)
              (with-temp-buffer
                (insert-file-contents foldfile)
                ;; if file is empty then return nil
                ;; `read' would not return nil, so we just check the buffer size
                (when (> (buffer-size) 0)
                  (read (current-buffer)))))))

      ;; uncollapse all headlines
      (when foldstates
        (show-all)
        (goto-char (point-min))

        ;; if file does not start with headline then jump to first headline
        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))

        (while (and foldstates (not (eobp)))
          ;; read first element from list, and if `t' then collape headline
          (when (pop foldstates)
            (hide-subtree))

          ;; continue until all foldstates list is empty (I think if things work
          ;; correctly, then check for (eobp) should be redundant here)
          (outline-next-visible-heading 1))

        (message "restored saved folding")))))

;; when org-mode buffer, then call orgfold-activate
(add-hook 'org-mode-hook 'orgfold-activate)

(defun orgfold-activate ()
  ;; call the restore function
  (orgfold-restore)
  ;; and add a local hook, to save the foldstates
  ;; (only is buffer was saved, see `orgfold-kill-buffer')
  (add-hook 'kill-buffer-hook 'orgfold-kill-buffer nil t))

(defun orgfold-kill-buffer ()
  ;; don't save folding info for unsaved buffers
  (unless (buffer-modified-p)
    (orgfold-save)))
