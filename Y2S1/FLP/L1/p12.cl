; a)
; dotProduct(a1..am, b1..bn) =
;   { 0, if m = ∅ or n = ∅
;   ; a1 * b1 + dotProduct(a2..am, b2..bn), otherwise
;   }
(defun dot-product (as bs)
  (cond
    ((or (null as) (null bs)) 0)
    (t (+ (* (car as) (car bs)) (dot-product (cdr as) (cdr bs))))))

; b)
; deepMax(a1..an) =
;   { 0, if a = ∅
;   ; max(a1, deepMax(a2..an)), if a1 is number
;   ; max(deepMax(a1), deepMax(a2..an)), otherwise
;   }
(defun deep-max (as)
  (cond
    ((null as) 0)
    ((numberp (car as)) (max (car as) (deep-max (cdr as))))
    (t (max (deep-max (car as)) (deep-max (cdr as))))))

; c)
; arithOp?(op) = op ϵ { +, -, *, / }
(defun arith-op? (op) (member op '(+ - * /)))

; arithOp(op) = ???
(defun arith-op (op) (nth (position op '(+ - * /)) (list #'+ #'- #'* #'/)))

;
(defun eval-arith-mvb (as)
  (cond
    ((null as) 0)
    ((numberp (car as)) (values (car as) (cdr as)))
    ((arith-op? (car as))
      (multiple-value-bind (a rest) (eval-arith-mvb (cdr as))
        (multiple-value-bind (b rest2) (eval-arith-mvb rest)
          (values (apply (arith-op (car as)) (list a b)) rest2))))))

(defun eval-arith (as) (multiple-value-bind (a rest) (eval-arith-mvb as) a))

; d)
; evenCount(a1..an) = n = ∅ or (n >= 2 and evenCount(a3..an))
(defun even-count (as)
  (or
    (null as)
    (and
      (consp as)
      (consp (cdr as))
      (even-count (cdr (cdr as))))))
