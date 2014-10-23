
lumns(m)
	(loop for i from 0 to m do 
		(princ "+")
		(princ "    ")


	)
)

(defun printboard(n m in-file)
	(loop for i from 0 to n do 
		(printcolumns m)
		(terpri)
		(setq lst (createlist in-file m 'nil))
		(printnumbers lst  0)
		(terpri)


	)
)

(defun printnumbers(lst num)
	(cond
		((eq lst nil) lst)
		((eq num 0)(princ "  ")(princ (car lst))(printnumbers (cdr lst) (1+ num)))
		(t (princ "    ")(princ (car lst)) (printnumbers (cdr lst) (1+ num)))
	
	)


)



(defun readfromfile()
(setq in-file (open "input.dat" :direction :input))
(setq row (read in-file))
(setq column (read in-file))
;(printboard row column in-file)
(setq counterlist (makeemptylist row column))
(princ counterlist)
(close in-file)
)


(defun makemylist(in-file n m lst)

	(setq lst (append (list (createlist in-file m nil))))
	(loop for i from 1 to (1- n) do
	 (push (createlist in-file m nil) (cdr (last lst))) 
			
	)
	 lst
)


(defun readfile (infile)
       (do ((num (read infile nil 'eof) (read infile nil 'eof)))
         ((eql num 'eof) 'done)   ;; End of loop test
         (format t "~a~%" num)
       )
   
)



(defun createList(in-file m lst)
	(setq cha (read in-file))
	(cond
		    ((eq m 0) lst)
		     ((eq cha 'eof) (princ "end of file reached"))
		    ((eq lst nil) (setq lst (list cha)) (createlist in-file (1- m) lst))			
		    (t (setq lst (append lst (list cha))) (createlist in-file (1- m) lst))
		    
	)
	
)

(defun makeemptylist(n m)
(setq lst (make-list m :initial-element '0))
(setq emptylist (make-list n :initial-element lst))
emptylist
)

(defun returnnth(n m lst)

(setq sublst (nth n lst))
(setq element (nth m sublst))
element 

)


(defun increamentcounter(n m row column command couterlist)
	
	(setq counterlist (increamentposition n m counterlist))
	(cond
		((eq t (and (not(eq m 0)) (eq command 'l) )) (setq counterlist (increamentposition n (1- m) counterlist)))
		;((and (not(eq m (1- column))) (eq command 'r)) (setq counterlist (increamentposition n (1+ m) counterlist)))
		;((and (not (eq n 0)) ( eq command 'u)) (setq counterlist (increamentposition (1- n) m counterlist )))
		;((and (not (eq n (1- row))) (eq command d)) (setq counterlist (increamentposition (1+ n) m counterlist)) )
		(t counterlist)
	
	)
)


(defun increamentposition(n m lst)

	(setf (nth m (nth n lst)) (1+ (nth m (nth n lst))) )
	lst
)
