;; for getting a value `let-alist' provides a more convenient alternative
(defun al-get (alist &rest keys)
  (dolist (k keys alist)
    (setq alist (alist-get k alist))))

(al-get '((a (b c d))) 'a 'b)

;; for setting a value, I am not aware of good alternatives
(defun al-set (alist val &rest keys)
  (let* ((current-key (pop keys))
         (sublist (cl-pushnew (list current-key) alist :key #'car)))
    (dolist (k keys)
      (setq sublist (cl-pushnew (list k) (alist-get current-key sublist) :key #'car))
      (setq current-key k))
    (when val
      (setf (alist-get current-key sublist) val))
    alist))

(let ((tl '((a (b (c) (d))))))
  (al-set tl "hoi" 'a 'b 'c))

;; we can use a simple macro to implement the destructive version
(defmacro al-set! (alist val &rest keys)
  `(setq ,alist (al-set ,alist ,val ,@keys)))

(setq tl nil)
(al-set! tl 'test 'a 'b 'c)

(let ((sub (al-get tl 'a 'b)))
  (setf (alist-get 'c sub) 'tost))

;;;; Below are previously found solutions (all less clean than the code above)

;; This seems to be the cleanest implementation, found at (slightly adapted)
;; https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/recursive-assoc-and-subst/m-p/7852434/highlight/true#M366429
;; however, this does not insert the new cons if it does not yet exist (but it
;; could be adapted for that)
(setq lst '((1 . "a")
	          (2 ((21 . "b") (22 . "c")))
	          (3 ((31 . "d") (32 ((321 . "x") (322 . "y")))))))

(defun recassoc (lst &rest keys)
  (cond
   ((null lst) nil)
   ((null (cdr keys)) (assoc (car keys) lst))
   ((and (setq sub (assoc (car keys) lst))
	       (listp (cadr sub)))
    (apply #'recassoc (cadr sub) (cdr keys)))))

(cdr (recassoc lst 2 21))

(defun recsubst	(val lst &rest keys)
  (cond
   ((null lst) nil)
   ((null (cdr keys))
    (subst (cons (car keys) val) (assoc (car keys) lst) lst))
   ((equal (car keys) (caar lst))
    (if (listp (cdar lst))
        (cons (cons (caar lst) (list (apply #'recsubst val (cadar lst) (cdr keys)))) (cdr lst))
      lst))
   (t (cons (car lst) (apply #'recsubst val (cdr lst) keys)))))

(recsubst "e" lst 2 21)


;; My universal 'destructive' macro solution (also adds if not yet exists).
;; Well, this solution looks clean to me also (maybe cleaner as an expanded
;; macro in the end is not recursive?)
(setq test '((a (b (c e)) (c (d f))) (c d)))

(defmacro aget (alist &rest keys)
  (let ((args (reverse keys)))
    (if (cdr args)
        `(alist-get ,(car args) (aget ,alist ,@(seq-subseq keys 0 -1)) nil nil #'equal)
      `(alist-get ,(car args) ,alist nil nil #'equal))))

(aget test 'a)
(aget test 'c)

(setf (aget test 'c) 'test)
(setf (aget test 'a 'c 'd) 'test)
(setf (aget test 'a 'c 'e) 'test)
test


;; let-alist solution (works if keys are symbols only)
(setf (car (let-alist test .a.b.c)) 'f)
test



;; Alternative solution (implied from Paul Graham's 'On lisp')
;; requires adding a case for each level
;; (setq test '((a ((b ((c e))))) (c d)))

;; (defmacro aget (alist &rest args)
;;   (pcase (length args)
;;     (1 `(car (alist-get ',(car args)
;;                      ,alist)))
;;     (2 `(car (alist-get ',(cadr args)
;;                          (car (alist-get ',(car args) ,alist)))))
;;     (3 `(car (alist-get ',(caddr args)
;;                          (car (alist-get ',(cadr args)
;;                                           (car (alist-get ',(car args) ,alist)))))))))

;; (aget test a b)
;; (aget test a b c)
;; (setf (aget test a b c) 'f)
;; test
