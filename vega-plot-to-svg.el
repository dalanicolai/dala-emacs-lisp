(defun vega-view-to-svg (file spec)
  "Use vega-lite to plot SPEC to FILE.
When file is nil, simply return svg definition as string."
  (when (and file (not (string= (file-name-extension file) "svg")))
    (user-error "File should be .svg file"))
  (let ((svg (with-temp-buffer
               (insert (json-encode spec))
               (let ((code (call-process-region (point-min) (point-max)
                                                "vl2svg" t t nil)))
                 (if (= code 0)
                     (buffer-string)
                   (user-error "Error during creating plot"))))))
    (cond (file (with-temp-file file (insert svg))
                file)
          (t svg))))

(progn (pop-to-buffer "*vega*")
       (insert (vega-view-to-svg nil '((data
					(values ((a . "C") (b . 2))
						((a . "C") (b . 7))
						((a . "C") (b . 4))
						((a . "D") (b . 1))
						((a . "D") (b . 2))
						((a . "D") (b . 6))
						((a . "E") (b . 8))
						((a . "E") (b . 4))
						((a . "E") (b . 7))))
				       (mark . "point")
				       (encoding (x (field . "a") (type . "nominal"))))))
       (image-mode))

(vega-view-to-file "vega_plot.svg" '(($schema . "https://vega.github.io/schema/vega-lite/v4.json")
                                     (description . "Plots two functions using a generated sequence.")
                                     (width . 300)
                                     (height . 150)
                                     (data (sequence (start . 0) (stop . 12.7) (step . 0.1) (as . "x")))
                                     (transform ((calculate . "sin(datum.x)") (as . "sin(x)"))
                                                ((calculate . "cos(datum.x)") (as . "cos(x)"))
                                                ((fold "sin(x)" "cos(x)")))
                                     (mark . "line")
                                     (encoding (x (type . "quantitative")
                                                  (field . "x"))
                                               (y (field . "value")
                                                  (type . "quantitative"))
                                               (color (field . "key")
                                                      (type . "nominal")
                                                      (title . :null)))))

(vega-view-to-svg "vega_plot.svg" '(($schema . "https://vega.github.io/schema/vega-lite/v4.json")
                                    (description . "Plots two functions using a generated sequence.")
                                    (width . 300)
                                    (height . 150)
                                    (data (values ((a . 1) (b . 2))
                                                  ((a . 2) (b . 7))
                                                  ((a . 3) (b . 4))
                                                  ((a . 4) (b . 1))
                                                  ((a . 5) (b . 2))
                                                  ((a . 6) (b . 6))
                                                  ((a . 7) (b . 8))
                                                  ((a . 8) (b . 4))
                                                  ((a . 9) (b . 7))))
                                    (mark . "line")
                                    (encoding (x (type . "quantitative")
                                                 (field . "a"))
                                              (y (field . "b")
                                                 (type . "quantitative"))
                                              (color (field . "key")
                                                     (type . "nominal")
                                                     (title . :null)))))
