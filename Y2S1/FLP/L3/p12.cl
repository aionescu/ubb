(defun curry (f &rest initial)
  (lambda (&rest args)
    (apply f (append initial args))))

(defun replace-n (old new l)
  (cond
    ((eql old l) new)
    ((atom l) l)
    (t (mapcar (curry #'replace-n old new) l))))
    ; (t (mapcar (lambda (x) (replace-n old new x)) l))))
