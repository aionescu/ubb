; (A (B) (C (D) (E)))

; get-level(x, t1..tn) {
;   nil, if n = 0
;   0, if x = t1
;   get-level(x, t2) + 1 if get-level(x, t2) is not nil
;   get-level(x, t3) + 1 if get-level(x, t3) is not nil
;   nil, otherwise
; }
(defun get-level (node tree)
  (cond
    ((null tree) nil)
    ((eql (car tree) node) 0)
    (t
      (let ((left (get-level node (cadr tree))))
        (cond
          ((not (null left)) (+ left 1))
          (t
            (let ((right (get-level node (caddr tree))))
              (cond
                ((null right) nil)
                (t (+ right 1))))))))))

; (get-level 'A '(A (B) (C (D) (E))))
