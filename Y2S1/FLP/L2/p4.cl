; my-append(a1..an, b1..bm) {
;   b1..bm, if n == 0
;   a1 U my-append(a2..an, b1..bm), otherwise
; }
(defun my-append (a b)
  (cond
    ((null a) b)
    (t (cons (car a) (my-append (cdr a) b)))))

; from (A (B) (C (D) (E)))
; to (A 2 B 0 C 2 D 0 E 0)

; (A) -> (A 0)
; (A (...)) -> (A 1 ...)
; (A (...) (...)) -> (A 2 ... ...)

; convert(t1..tn) {
;   [t1, 0], if n == 1
;   t1 U 1 U convert(t2..tn), if n == 2
;   t1 U 2 U my-append(convert(t2), convert(t3)), otherwise
; }
(defun convert (tree)
  (cond
    ((null (cdr tree)) (list (car tree) 0))
    ((null (cddr tree)) (cons (car tree) (cons 1 (convert (cdr tree)))))
    (t (cons (car tree) (cons 2 (my-append (convert (cadr tree)) (convert (caddr tree))))))))
