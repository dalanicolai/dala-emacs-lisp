;;; ppm-gen-simple.el --- Simple ppm generator       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; Keywords: 

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

(defun ppm-create (width height &optional color depth)
  (let (rows)
    (setq depth (or depth (1- (expt 2 8))))
    (when color
      (setq color (vconcat (mapcar (lambda (v) (round (* v depth)))
                                   (pcase color
                                     ((pred stringp) (color-name-to-rgb color))
                                     (_ color))))))
    (dotimes (_ height)
      (push (make-vector width (or color [0 0 0])) rows))
    (list :width width :height height :depth depth :data (vconcat rows))))

(defun ppm-plot (ppm x y rgb)
  (let ((depth (plist-get ppm :depth))
        (data (plist-get ppm :data)))
    (setq rgb (mapcar (lambda (v) (* v depth)) rgb))
    (setf (aref (aref data y) x) (vconcat rgb))))

(defun ppm-display (ppm)
  (interactive)
  (let ((w (plist-get ppm :width))
        (h (plist-get ppm :height))
        (depth (plist-get ppm :depth))
        (data (plist-get ppm :data)))
    (with-temp-buffer
      (insert (format "P3\n%d %d\n%d\n" w h depth))
      (mapc (lambda (row)
              (mapc (lambda (p)
                      (pcase-let ((`[,r ,g ,b] p))
                        (insert (format "%d %d %d\n" r g b))))
                    row))
            data)
      (buffer-string))))


(provide 'ppm-gen-simple)
;;; ppm-gen-simple.el ends here
