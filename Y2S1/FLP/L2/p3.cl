; depth-mvb(t1..tn) =
;   { (1, t3..tn), if t2 = 0
;   ; (depth + 1, rest) where (depth, rest) = depth-mvb(t3..tn), if t2 = 1
;   ; (max(depth1, depth2) + 1, rest2)
;       where (depth2, rest2) = depth-mvb(rest1)
;       where (depth1, rest1) = depth-mvb(t3..tn)
;       , otherwise
;   }
(defun depth-mvb (tree)
  (cond
    ((= (cadr tree) 0) (values 1 (cddr tree)))
    ((= (cadr tree) 1)
      (multiple-value-bind (depth rest) (depth-mvb (cddr tree))
        (values (+ 1 depth) rest)))
    (t
      (multiple-value-bind (depth1 rest1) (depth-mvb (cddr tree))
        (multiple-value-bind (depth2 rest2) (depth-mvb rest1)
          (values (+ 1 (max depth1 depth2)) rest2))))))

; depth(t1..tn) = depth where (depth, rest) = depth-mvb(t1..tn)
(defun depth (tree)
  (multiple-value-bind (depth rest) (depth-mvb tree) depth))
