;; This org file contains a translation from the ulisp article at http://www.ulisp.com/show?2NWA to elsip
;; Additionally this file uses ppm-gen.el from https://www.emacswiki.org/emacs/PpmGen for plotting

(require 'ppm-gen)
(require 'cl-lib)

(defun colour (r g b) (list r g b))
(defun pnt (x y z) (list x y z))
(defun vect (x y z) (list x y z))

(defun add (v w) (cl-mapcar #'+ v w))
(defun subtract (v w) (cl-mapcar #'- v w))

(defun mul (c v) (mapcar (lambda (x) (* c x)) v))
(defun dot (v w) (apply #'+ (cl-mapcar #'* v w)))

(defun square (x) (* x x))
(defun magnitude (v) (sqrt (apply #'+ (mapcar #'square v))))

(defun unit-vector (v)
  (let ((d (magnitude v)))
    (mapcar (lambda (j) (/ j d)) v)))

(defun distance (p1 p2)
  (magnitude (cl-mapcar #'- p1 p2)))

(defvar *world* nil)
(defvar *eye* nil)
(defvar *light* nil)

(defun sphere-center (s) (second s))
(defun sphere-radius (s) (third s))
(defun sphere-colour (s) (fourth s))

(defun plane-point (s)  (second s))
(defun plane-normal (s) (third s))
(defun plane-colour (s) (fourth s))

(defun make (&rest list)
  (push list *world*))

(setq magnification 1)

(setq *world* nil)
(setq *eye* (mul magnification (pnt 0.0 0.0 200.0)))
(setq *light* (mul magnification (pnt -5000 10000 -1200)))
(make 'plane (mul magnification (pnt 0 -200 0)) (vect 0 -1 0) (colour 2 2 2))
(make 'sphere (mul magnification (pnt -250 0 -1000)) (* magnification 200) (colour 0 1 .5))
(make 'sphere (mul magnification (pnt 50 0 -1200)) (* magnification 200) (colour 1 .5 0))
(make 'sphere (mul magnification (pnt 400 0 -1400)) (* magnification 200) (colour 0 .5 1))
(make 'sphere (mul magnification (pnt -50 -150 -600)) (* magnification 50) (colour 0 0 1))
(make 'sphere (mul magnification (pnt 200 -150 -800)) (* magnification 50) (colour 1 0 0))

(defun object-colour (s)
  (cl-case (car s)
    (sphere (sphere-colour s))
    (plane (plane-colour s))))

(defun object-normal (s pt)
    (cl-case (car s)
      (sphere (sphere-normal s pt))
      (plane (plane-normal s))))

(defun sphere-normal (s pt)
  (unit-vector (subtract (sphere-center s) pt)))

(defun object-hit (s pt pr)
  (cl-case (car s)
    (sphere (sphere-hit s pt pr))
    (plane (plane-hit s pt pr))))

(defun sphere-hit (s pt pr)
  (let* ((c (sphere-center s))
         (oc (cl-mapcar #'- pt c)))
    (minroot
     (apply #'+ (mapcar #'square pr))
     (* 2 (dot oc pr))
     (- (dot oc oc) (square (sphere-radius s))))))

(defun plane-hit (s pt pr)
  (let ((denom (dot (plane-normal s) pr)))
    (unless (zerop denom)
      (let ((n (/ (dot (subtract (plane-point s) pt) (plane-normal s)) denom)))
        (when (>= n 0) n)))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
    (let ((disc (- (square b) (* 4 a c))))
      (unless (minusp disc)
        (min (/ (+ (- b) (sqrt disc)) (* 2 a))
             (/ (- (- b) (sqrt disc)) (* 2 a)))))))

(setq *xres* (* magnification 160))
(setq *yres* (* magnification 128))
(setq img (ppm-make *xres* *yres*))

(defun tracer ()
  (dotimes (x *xres*)
    (dotimes (y *yres*)
      ;; (print (apply #'color-rgb-to-hex (colour-at (- x 80) (- 64 y)))))))
      (ppm-plot img x y (apply #'ppm-rgb (mul 255 (colour-at (- x 80) (- 64 y)))))))
  (ppm-show img))

            ;; (ppm-plot ppm x y (apply rgb (colour-at (- x 80) (- 64 y)))))))

(defun colour-at (x y)
  (let ((c (send-ray
            *eye*
            (unit-vector
             (subtract (list x y 0) *eye*)))))
    (or c (background x y))))

(defun background (x y) (colour 0.5 0.7 1))

(defun lambert (s hit pr)
  (max 0 (dot pr (object-normal s hit))))

(defun first-hit (pt pr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((n (object-hit s pt pr)))
        (when n
          (let* ((h (add pt (mul n pr)))
                 (d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setq surface s)
              (setq hit h)
              (setq dist d))))))
    (list surface hit)))

(defun send-ray (pt pr)
  (let* ((f (first-hit pt pr))
         (s (first f))
         (hit (second f)))
    (when s
      (let* ((c (mul (lambert s hit pr) (object-colour s)))
             (f2 (first-hit *light* (unit-vector (subtract hit *light*))))
             (h2 (second f2)))
        (cond
         ((< (distance hit h2) 1) c)
         (t (mul .75 c)))))))

(print (benchmark-run (tracer)))
