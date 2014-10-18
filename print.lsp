(defun printb(r)
(cond ((= 0 r) (princ " +"))
(t (princ " +") (printb(1- r))  )
))

(defun printcolumns(m)
(loop for i from 0 to m do (princ "+")(princ " "))
)

(defun printboard(n m)
(loop for i from 0 to n do (printcolumns m)(terpri))
)


