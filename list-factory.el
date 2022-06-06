;; -*- lexical-binding: t; -*-

(defun lf-enumerate (list &optional start-from)
  (let (alist
        (counter (or start-from 0)))
    (dolist (e list)
      (push (cons counter e) alist)
      (setq counter (1+ counter)))
    (nreverse alist)))

(lf-enumerate '(3 5 7))

(mapcar (lambda (c)
          (cons (car c) (cdr c)))
        (lf-enumerate '(3 5 7)))

;; (benchmark 2000000 '(lf-enumerate '(a b c)))

;; (defun lf-enumerate2 (l)
;;   (let ((i 0)
;;         alist)
;;     (while l
;;       (setq alist (cons (cons i (car l)) alist))
;;       (setq i (1+ i)
;;             l (cdr l)))
;;     alist))


;; inspired by `dolist' macro

;; (defmacro lf-enumerate (spec &rest body)
;;   "Enumerate over list

;; \(fn (VAR LIST [START RESULT]) BODY...)"
;;     (declare (indent 1) (debug ((symbolp symbolp form &optional numberp form) body)))
;;     (unless (consp spec)
;;       (signal 'wrong-type-argument (list 'consp spec)))
;;   (unless (<= 3 (length spec) 5)
;;     (signal 'wrong-number-of-arguments (list '(3 . 5) (length spec))))
;;   (let ((temp '--enumerate-tail--))
;;     (if lexical-binding
;;         `(let ((,temp ,(nth 2 spec))
;;                (,(car spec) (or ,(nth 3 spec) 0)))
;;            (while ,temp
;;              (let ((,(cadr spec) (car ,temp)))
;;                ,@body
;;                (setq
;;                 ,(car spec) (1+ ,(car spec))
;;                 ,temp (cdr ,temp))))
;;            ,@(cdr (cdr (cdr spec))))
;;       `(user-error "Usage of this macro requires lexical binding"))))

;; (let (alist)
;;   (lf-enumerate (i e '(a b c) 2 (nreverse alist))
;;     (push (cons i e) alist)))

(defun lf--range-type (start stop)
  (let ((type (type-of start)))
    (if (eq type (type-of stop))
        type
      (error "Elements must be of same type."))))

(defun lf--range-pre-convert (x)
  (pcase (type-of x)
    ('integer x)
    ('string (string-to-char x))))

(defun lf-range (from to &optional step)
  (let* ((type (lf--range-type from to))
         (beg (lf--range-pre-convert from))
         (end (lf--range-pre-convert to))
         (fn (pcase type 
               ('integer #'identity)
               ('string #'char-to-string))))
    (mapcar fn
            (number-sequence beg end step))))

(lf-range "a" "d")

(defun lf-array (x &optional to step)
  (if to
      (lf-range x to step)
    (lf-range (if (stringp x) "a" 1)
                x)))

(defmacro lf-arrays (&rest domain)
  `(mapcar (lambda (d)
             (lf-array (car d) (cadr d) (caddr d)))
           ,`(mapcar #'cdr ',domain)))

(lf-arrays (x 2 4 0.5) (y 3 5))


(defun lf-map-cons (fn cons)
  (cons (funcall fn (car cons))
        (funcall fn (cdr cons))))

(lf-map-cons (lambda (x) (* 2 x)) '(100 . 200))

(defun lf--outer-helper (flat vals &rest lists)
  (if (not (cdr lists))
      (mapcar (lambda (x)
                (reverse (cons x vals)))
              (car lists))
    (funcall (if flat #'mapcan #'mapcar)
             (lambda (x)
               (apply #'lf--outer-helper flat (cons x vals) (cdr lists)))
           (car lists))))

(defun lf-outer (&rest lists)
  (apply #'lf--outer-helper t nil lists))

(lf-outer '(a b) '(c d) '(g h))

(defun lf--table-flat-helper (fn &rest lists)
  (mapcar (lambda (x)
            (apply fn x))
          (apply #'lf-outer lists)))

(lf--table-flat-helper #'+
                       '(1 3) '(4 6) '(7 9))
(lf--table-flat-helper (lambda (x y z)
                         (+ x y z))
                       '(1 3) '(4 6) '(7 9))

(-table-flat (lambda (x y z)
                         (+ x y z))
                       '(1 3) '(4 6) '(7 9))

(defmacro lf-table-flat (fn &rest domain)
  `(apply #'lf--table-flat-helper
          (if (eq (car ',fn) 'function)
              ,fn
            (lambda ,(mapcar #'car domain) ,fn))
          (lf-arrays ,@domain)))

(lf-table-flat #'+ (x 3) (y 4 6 0.5))
(lf-table-flat (+ x y z) (x 1 3) (y 4 6) (z 7 9))

(defun lf-outer-nested (&rest lists)
  (apply #'lf--outer-helper nil nil lists))

(defun lf--table-helper (fn tree)
  (if (listp (car tree))
      (mapcar (lambda (l)
                (lf--table-helper fn l))
              tree)
    (apply fn tree)))

(lf--table-helper #'+ (lf-outer-nested '(1 2) '(3 4) '(5 6)))
(lf--table-helper (lambda (x y z)
                    (+ x y z))
                  (lf-outer-nested '(1 2) '(3 4) '(5 6)))
(-table (lambda (x y z)
          (+ x y z))
        '(1 2) '(3 4) '(5 6))

(lf--table-helper #'concat (lf-outer-nested '("a" "c") '("e" "f") '("g" "h")))

(defmacro lf-table (fn &rest domain)
  `(lf--table-helper
    (if (eq (car ',fn) 'function)
        ,fn
      (lambda ,(mapcar #'car domain) ,fn))
    (apply #'lf-outer-nested (lf-arrays ,@domain))))

(lf-table #'+ (x 3) (y 4 6 0.5))
(lf-table (+ x y z) (x 1 3) (y 4 6) (z 7 9))
(lf-table #'concat (x "a" "c") (y "r" "z") (z "B" "L"))

;; (defun lf--table (fn &rest domain)
;;   (let ((outer (mapcar #'list (car domain))))
;;     (mapc (lambda (x)
;;             (setq outer (lf-map-cons outer x)))
;;           (cdr domain))
;;     (mapcar (lambda (y) (apply fn y))
;;             (mapcar #'reverse outer))))

;; (lf--table (lambda (a b c)
;;              (when (= a 1) (+ a b c)))
;;            '(1 2) '(3 4) '(5 6))

;; (defmacro lf-table (fn &rest domain)
;;   (declare (debug (form &rest form)))
;;   `(let ((x ',(mapcar (lambda (y)
;;                         (if (listp (cadr y))
;;                             (if (fboundp (caadr y))
;;                                 (eval (cadr y))
;;                               (cadr y))
;;                           (if (boundp (cadr y))
;;                               (eval (cadr y))
;;                           (apply #'lf-array (cdr y)))))
;;                       domain)))
;;      (apply #'lf--table (lambda ,(mapcar #'car domain) ,fn)
;;                 x)))

;; (lf-table (cons a b) (a '(1 2)) (b (buffer-list)))
;; (delq nil (lf-table (unless (= a 1) (+ a b c)) (a 1 5) (b 3) (c 2 8)))

;; (ert-deftest lf-array-test ()
;;   (should (equal (lf-array 3) '(1 2 3)))
;;   (should (equal (lf-array "c") '("a" "b" "c")))
;;   (should (equal (lf-array 2 4) '(2 3 4)))
;;   (should (equal (lf-array "b" "d") '("b" "c" "d")))
;;   (should (equal (lf-array 1 5 2) '(1 3 5)))
;;   (should (equal (lf-array "a" "e" 2) '("a" "c" "e")))
;;   (should (equal (lf-array "z" "A" -20) '("z" "f" "R")))
;;   (should (equal (lf-table (concat a b c) (a "c") (b "Z" "Y" -1) (c "d" "e"))
;;                  '("aZd" "bZd" "cZd" "aYd" "bYd" "cYd" "aZe" "bZe" "cZe" "aYe" "bYe" "cYe")))
;;   (should (equal (lf-table (unless (= b 9)(+ a b c)) (a 8) (b 9 7 -2) (c 6 7))
;;                  '(16 17 18 19 20 21 22 23 14 15 16 17 18 19 20 21 17 18 19 20 21 22 23 24 15 16 17 18 19 20 21 22))))
