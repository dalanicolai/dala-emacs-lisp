(require 'which-key nil t)

;;; Custom variables
(defun sbj-get-emacs-configuration ()
  (if (boundp 'spacemacs-default-map)
      spacemacs-default-map
    global-map))


;; REMOVE THIS VARIABLE AND SET KEYBINDING DIRECTLY
;; ADAPT AND MOVE DOCSTRING TO sbj-assign-buffer-shortcut FUNCTION
(defcustom sbj-buffer-shortcut-set (if (boundp 'spacemacs-default-map)
                                  "bjj"
                                  ("C-c b"))
  "Keysequence to set a buffer shortcut.
The buffer can then be jumped to (i.e. selected) by typing the
keys defined with the variable `sbj-buffer-jump-prefix' followed
by the character assigned here")

(defcustom sbj-buffer-jump-prefix (if (boundp 'spacemacs-default-map)
                                  "bj"
                                  ("C-c w"))
  "Prefix to use for simple buffer jump.
Quickly jump to the buffer typing the prefix set here followed by
the character assigned to the buffer via
\\[sbj-assign-buffer-shortcut]")

(defcustom sbj-keymap (sbj-get-emacs-configuration)
  "Keymap to which keybindings should be added.
Generally the value `global-map' for Emacs and
`spacemacs-default-map' for Spacemacs is recommended. The default
setting is to set the map automatically.")

(defcustom sbj-buffer-shortcut-set (sbj-buffer-shortcut-set)
  "Keysequence to set buffer shortcut.")

(defcustom sbj-buffer-jump-prefix (sbj-buffer-jump-prefix)
  "Prefix keys for jump to buffer.")

;;; Code
(setq sbj-buffer-binding-plist nil)

(defun sbj-assign-buffer-shortcut (char)
  (interactive "cAssign character for jumping to current buffer: ")
  (unless (eq char '27)
    (let ((key (char-to-string (eval char))))
      (setq sbj-buffer-binding-plist
            (plist-put sbj-buffer-binding-plist
                       (intern key)
                       (buffer-name (current-buffer))))
      ;; Insert/replace if emacs/spacemacs
      (define-key sbj-keymap
        (format "%s%s" sbj-buffer-jump-prefix key)
             (list 'lambda '() '(interactive) (list 'sbj-jump-buffer (eval key))))
       (when (fboundp 'which-key-add-key-based-replacements)
        (which-key-add-keymap-based-replacements sbj-keymap
          (format "%s%s" sbj-buffer-jump-prefix key) (buffer-name (current-buffer))))
      (message (format "Key '%s' assigned to buffer with name '%s'" key (buffer-name (current-buffer)))))))

(defun sbj-jump-buffer (char)
  (interactive "cType shortcut key to buffer: ")
  (if (get-buffer (plist-get sbj-buffer-binding-plist (intern char)))
      (switch-to-buffer (plist-get sbj-buffer-binding-plist (intern char)))
    (message "Buffer has been killed or renamed")))

;;; Keybindings
(spacemacs/declare-prefix sbj-buffer-jump-prefix "jump-to-buffer")
(spacemacs/set-leader-keys sbj-buffer-shortcut-set 'sbj-assign-buffer-shortcut)
