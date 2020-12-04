; a)
; dot-product(a1..am, b1..bn) =
;   { 0, if m = 0 or n = 0
;   ; a1 * b1 + dot-product(a2..am, b2..bn), otherwise
;   }
(defun dot-product (as bs)
  (if (or (null as) (null bs))
    0
    (+ (* (car as) (car bs)) (dot-product (cdr as) (cdr bs)))))

(defun dot-product-2 (as bs) (reduce #'+ (mapcar #'* as bs)))

; b)
; deep-max(a1..an) =
;   { 0, if a = ∅
;   ; max(a1, deep-max(a2..an)), if a1 is number
;   ; max(deep-max(a1), deep-max(a2..an)), otherwise
;   }
(defun deep-max (as)
  (cond
    ((null as) 0)
    ((numberp (car as)) (max (car as) (deep-max (cdr as))))
    (t (max (deep-max (car as)) (deep-max (cdr as))))))

(defun max-helper (a) (if (listp a) (deep-max-2 a) a))
(defun deep-max-2 (as) (if (null as) 0 (reduce #'max (mapcar #'max-helper as))))

; c)
; op?(op) = op ϵ { +, -, *, / }
(defun op? (op) (member op '(+ - * /)))

; op(op) =
;   { (+), if op is +
;   ; (-), if op is -
;   ; (*), if op is *
;   ; (/), if op is /
;   }
(defun op (op) (nth (position op '(+ - * /)) (list #'+ #'- #'* #'/)))

; eval-arith-mvb(a1..an) =
;   { 0, if n = 0
;   ; (a1, a2..an), if a1 is a number
;   ; (op(a1)(a, b), rest2), if op?(a1), (a, rest) = eval-arith-mvb(a2..an), (b, rest2) = eval-arith-mvb(rest2)
(defun eval-arith-mvb (as)
  (cond
    ((null as) (values 0 nil))
    ((numberp (car as)) (values (car as) (cdr as)))
    ((op? (car as))
      (multiple-value-bind (a rest) (eval-arith-mvb (cdr as))
        (multiple-value-bind (b rest2) (eval-arith-mvb rest)
          (values (funcall (op (car as)) a b) rest2))))))

; eval-arith(a1..an) = a, where (a, rest) = eval-arith-mvb(a1..an)
(defun eval-arith (as) (multiple-value-bind (a rest) (eval-arith-mvb as) a))

; d)
; even-count(a1..an) = n = ∅ or (n >= 2 and even-count(a3..an))
(defun even-count (as)
  (or
    (null as)
    (and
      (consp as)
      (consp (cdr as))
      (even-count (cdr (cdr as))))))
