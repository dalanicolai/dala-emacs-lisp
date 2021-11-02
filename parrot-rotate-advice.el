;; -*- lexical-binding: t; -*-

;; adding a separate dir requires to modifyin parrot-rotate.el i.e. appending
;; the list to `parrot-rotate-dict' in `parrot-rotate-get-rots-for' and
;; `parrot-rotate-word-at-point'
;; (defvar-local parrot-rotate-local-dict nil)
(defvar-local parrot-rotate-dict parrot-rotate-dict)

(defun parrot-replace-to-dict (&rest _)
  (when (eq this-command 'evil-visual-paste)
    (let* ((replaced (buffer-substring-no-properties evil-visual-beginning
                                                     evil-visual-end))
           (pasted (substring-no-properties (current-kill 1))))
      (let ((match 0))
        (dolist (x parrot-rotate-dict parrot-rotate-dict)
          (when (member replaced (cadr x))
            (setq match (1+ match))
            match
            (setf (cadr x) (append (list replaced pasted)
                                   (remove pasted (remove replaced (cadr x)))))))
        (dolist (x parrot-rotate-dict parrot-rotate-dict)
          (cond ((and (member pasted (cadr x))
                      (not (member replaced (cadr x))))
                 match
                 (if (= match 0)
                     (setf (cadr x) (append (list replaced pasted)
                                            (remove pasted (remove replaced (cadr x)))))
                   (let ((previous-val (cadr x)))
                     (setf (cadr x) (remove pasted (cadr x)))
                     (if (> (length (cadr x)) 1)
                         (warn (format "Both the replaced and the pasted pattern found in different
entries of `parrot-rotate-list', the pasted pattern has been removed from `%S'" previous-val))
                       (setq parrot-rotate-dict
                             (seq-remove (lambda (x) (= (length (cadr x)) 1)) parrot-rotate-dict))
                       (warn "Both the replaced and the pasted pattern found in different
entries of `parrot-rotate-list', the entry `%S' has been removed from the dict" previous-val)))))))))))

(advice-add 'evil-delete :before 'parrot-replace-to-dict)

(define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
(define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)

(setq parrot-rotate-dict
  '(
    (:rot ("begin" "end") :caps t :upcase t)
    (:rot ("enable" "disable") :caps t :upcase t)
    (:rot ("enter" "exit") :caps t :upcase t)
    (:rot ("forward" "backward") :caps t :upcase t)
    (:rot ("front" "rear" "back") :caps t :upcase t)
    (:rot ("get" "set") :caps t :upcase t)
    (:rot ("high" "low") :caps t :upcase t)
    (:rot ("in" "out") :caps t :upcase t)
    (:rot ("left" "right") :caps t :upcase t)
    (:rot ("min" "max") :caps t :upcase t)
    (:rot ("on" "off") :caps t :upcase t)
    (:rot ("prev" "next"))
    (:rot ("start" "stop") :caps t :upcase t)
    (:rot ("true" "false") :caps t :upcase t)
    (:rot ("&&" "||"))
    (:rot ("==" "!="))
    (:rot ("." "->"))
    (:rot ("if" "else" "elif"))
    (:rot ("ifdef" "ifndef"))
    (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
    (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
    (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
    (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))
    (:rot ("api" "napi"))
    ))

3rd

(let* ((d parrot-rotate-dict)
       (replaced (current-kill 0))
       (pasted (current-kill 1))
       (member (member "in" (print (plist-get (car d) :rot)))))
  (while (and (not member) d)
    (setq d (cdr d))
    (when (member "in" (print (plist-get (car d) :rot)))
      (setq member t)))
  (car d))

(mapcar (lambda (r) (cadr r))
        parrot-rotate-dict)

(setq test '(:rot "hello"))
( 'test)

(print "test")

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (push '(?p . ("(print " . ")")) evil-surround-pairs-alist)))
<p class="important">
({ Hello } world!)
</p>
