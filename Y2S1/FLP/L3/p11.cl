(defun list-depth (l)
  (cond
    ((atom l) 0)
    (t (+ 1 (apply #'max (mapcar #'list-depth l))))))

; (list-depth '(1 2 3 (4 5 6) (7) (8 (9)))) => 3
