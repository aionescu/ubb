(defun $ (f &rest initial)
  (lambda (&rest args)
    (apply f (append initial args))))

; replace-n(old, new, l) =
;   { new, if l = old
;   ; l, if l is an atom
;   ; ⋃(i=1,n) replace-n(old, new, li), otherwise
;   }
(defun replace-n (old new l)
  (cond
    ((eql old l) new)
    ((atom l) l)
    (t (mapcar ($ #'replace-n old new) l))))
    ; (t (mapcar (lambda (x) (replace-n old new x)) l))))
