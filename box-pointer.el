;;; box-pointer.el --- Box pointer notation in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; Keywords: tools

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

;; TODO create svg group element (see e.g. sketch-mode)

(defcustom bp-box-size 60
  "Box size")

(defun bp-boxes (list)
  (let (positions)
    (cl-labels ((box-positions (tree x y)
                  (cond ((consp tree)
                         (push (list 'dbox x y) positions)
                         (box-positions (car tree)
                                        x
                                        (+ y (/ (* 3 bp-box-size)
                                                2)))
                         (box-positions (cdr tree)
                                        (+ x (/ (* 5 bp-box-size) 2))
                                        y))
                        ;; (t (box-positions (car tree) x y)))))
                        (t (push (list 'box x y tree)  positions)))))
      (let ((start (/ bp-box-size 2)))
        (box-positions list start start)))
    positions))

(defun svg-box (svg x y &optional label)
  (let ((half-size (/ bp-box-size 2))
        (offset (/ bp-box-size 6)))
    (svg-rectangle svg
                   (- x half-size) (- y half-size)
                   bp-box-size bp-box-size)
    (svg-text svg (symbol-name label)
              :x (- x offset) :y (+ y offset)
              :font-size 20
              :fill "black"
              :font-family "impact")))

(defun svg-dbox (svg x y &optional label)
  (let ((half-size (/ bp-box-size 2)))
    (svg-rectangle svg
                   (- x half-size) (- y half-size)
                   (* 2 bp-box-size) bp-box-size)
    (svg-line svg
              (+ x half-size) (- y half-size)
              (+ x half-size) (+ y half-size))
    (svg-circle svg x y 2 :fill "black")
    (svg-line svg x y x (+ y bp-box-size))
    (svg-circle svg (+ x bp-box-size) y 2 :fill "black")
    (svg-line svg (+ x bp-box-size) y (+ x (* 2 bp-box-size)) y)))

(defun bp-draw (list)
  (let* ((boxes (bp-boxes list))
         (svg (svg-create 600 400 :fill "white" :stroke-color "black")))
    (svg-rectangle svg 0 0 600 400)
    (dolist (b (print boxes))
      (case (car b)
        ('box (apply #'svg-box svg (cdr b)))
        ('dbox (apply #'svg-dbox svg (cdr b)))))
    (svg-insert-image svg)))

(bp-draw '(a (b c)))

(provide 'box-pointer)
;;; box-pointer.el ends here
