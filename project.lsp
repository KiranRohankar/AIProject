(defun linetolist(string)"creates list from string"
  (loop :for (integer position) := (multiple-value-list 
                                    (parse-integer string
                                                   :start (or position 0)
                                                   :junk-allowed t))
        :while integer
        :collect integer)
)


(defun printcolumns(m) "prints default columns"
	(loop for i from 0 to m do 
		(princ "+")
		(princ "    ")


	)
)

(defun printboard(n m lst) "prints the starting board as per list"
	(loop for i from 0 to n do 
		(printcolumns m)
		(terpri)
		(printnumbers (nth i lst)  0)
		(terpri)


	)
)

(defun printnumbers(lst num) "print numbers in the boxes"
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
(readfile in-file)
(close in-file)
)


(defun listlinebyline(in-file m n lst)"reads input line by line and creates initial list"
	(setq lst (list (linetolist (read-line infile))))
	(
		loop for i from 1 to (1- n) do 
		(push ( linetolist (read-line infile)) (cdr (last lst)))
	)

	lst


)

(defun makemylist(in-file n m lst)

	(setq lst (append (list (createlist in-file m nil))))
	(loop for i from 1 to (1- n) do
	 (push (createlist in-file m nil) (cdr (last lst))) 
			
	)
	 lst
)






(defun createList(in-file m lst)"read m integer characters from the file "
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


(defun increamentcounter(n m row column command flag counterlist)
	
	(increamentposition n m counterlist)
	(cond
		((eq t (and (not(eq m 0)) (eq command 'l) (eq flag nil) ))(setq flag t)  (increamentposition n (1- m) counterlist) )
		((eq t (and (not(eq m (1- column))) (eq command 'r)(eq flag nil) )) (increamentposition n (1+ m) counterlist))
		((eq t (and (not (eq n 0)) ( eq command 'u)(eq flag nil) ))  (increamentposition (1- n) m counterlist ))
		((eq t (and (not (eq n (1- row))) (eq command 'd) (eq flag nil)))  (increamentposition (1+ n) m counterlist) )
		(t counterlist)
	
	)
)


(defun increamentposition(n m lst)

	(setf (nth m (nth n lst)) (1+ (nth m (nth n lst))) )
	lst
)


(defun singlelist(lst m)
	(cond
	((eq m 0)  lst)
	( t  (setq lst(append lst '(0))) (singlelist lst (1- m)))
		
	)

)

(defun doublelist(lst n m)

	(cond
		((eq n 0) lst)
		(t (setq ls(singlelist nil m )) (setq lst (append lst (list ls))) (doublelist lst (1- n) m))
	)
)
