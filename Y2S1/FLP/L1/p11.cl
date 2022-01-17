; a)

(defun my-max (a b)
  (cond
    ((null a) b)
    ((null b) a)
    ((> a b) a)
    (t b)))

; lcmHelper(max, a, b) {
;   max, if max % a = 0 and max % b = 0
;   lcmHelper(max + 1, a, b), otherwise
; }
(defun lcm-helper (max a b)
  (cond
    ((and (= (mod max a) 0) (= (mod max b) 0)) max)
    (t (lcm-helper (+ max 1) a b))))

(defun my-lcm (a b) (lcm-helper (my-max a b) a b))

; lcmList(l1..ln) {
;   1, if n = 0
;   myLcm(lcmList(l1), lcmList(l2..ln)), if l1 is a list
;   myLcm(l1, lcmList(l2..ln)), otherwise
; }
(defun lcm-list (l)
  (cond
    ((null l) 1)
    ((listp (car l)) (my-lcm (lcm-list (car l)) (lcm-list (cdr l))))
    (t (my-lcm (car l) (lcm-list (cdr l))))))

; b)
; descending(l1..ln) {
;   true, if n = 0 or n = 1
;   l1 > l2 and descending(l2..ln), otherwise
; }
(defun descending (l)
  (cond
    ((null l) t)
    ((null (cdr l)) t)
    (t (and (> (car l) (cadr l)) (descending (cdr l))))))

; mountain(l1..ln) {
;   false, if n = 0 or n = 1
;   mountain(l2..ln), if l1 < l2
;   descending(l2..ln), if l1 > l2
;   false, otherwise
; }
(defun mountain (l)
  (cond
    ((null l) nil)
    ((null (cdr l)) nil)
    ((< (car l) (cadr l)) (mountain (cdr l)))
    ((> (car l) (cadr l)) (descending (cdr l)))
    (t nil)))

; c)
; maxList(l1..ln) {
;   false, if n = 0
;   myMax(maxList(l1), maxList(l2..ln)), if l1 is a list
;   myMax(l1, maxList(l2..ln)), otherwise
; }
(defun max-list (l)
  (cond
    ((null l) nil)
    ((listp (car l)) (my-max (max-list (car l)) (max-list (cdr l))))
    (t (my-max (car l) (max-list (cdr l))))))

; removeList(e, l1..ln) {
;    [], if n = 0
;    removeList(e, l1) U removeList(e, l2..ln), if l1 is a list
;    removeList(e, l2..ln), if l1 = e
;    l1 U removeList(e, l2..ln), otherwise
; }
(defun remove-list (e l)
  (cond
    ((null l) nil)
    ((listp (car l)) (cons (remove-list e (car l)) (remove-list e (cdr l))))
    ((= e (car l)) (remove-list e (cdr l)))
    (t (cons (car l) (remove-list e (cdr l))))))

(defun remove-max-list (l) (remove-list (max-list l) l))

; d)
; evenProduct(l1..ln) {
;   1, if n = 0
;   l1 * evenProduct(l2..ln), if l1 is a number and l1 divisible by 2
;   evenProduct(l1) * evenProduct(l2..ln), if l1 is a list
;   evenProduct(l2..ln), otherwise
; }
(defun even-product (l)
  (cond
    ((null l) 1)
    ((and (numberp (car l)) (= (mod (car l) 2) 0)) (* (car l) (even-product (cdr l))))
    ((listp (car l)) (* (even-product (car l)) (even-product (cdr l))))
    (T (even-product (cdr l)))))
