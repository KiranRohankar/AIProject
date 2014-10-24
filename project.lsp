(defun linetolist(mystring)"creates list from string"
  (loop :for (integer position) := (multiple-value-list 
                                    (parse-integer mystring
                                                   :start (or position 0)
                                                   :junk-allowed t))
        :while integer
        :collect integer)
)

 (defun string-to-list (str)      
  (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil))
)


(defun printcolumns(m) "prints default columns"
	(loop for i from 0 to m do 
		(princ "+")
		(princ "    ")


	)
)

(defun printboard(n m lst lar uad) "prints the starting board as per list"
	(loop for i from 0 to n do 
		(printcolumns m)
		(terpri)
		(printnumbers (nth i lst)  0 (nth i lar))
		(terpri)


	)
)

(defun printnumbers(lst num lar) "print numbers in the boxes"
	(cond
		((eq lst nil) lst)
	;	((eq (car lar) 'true) (princ "|"))
		((eq num 0)(princ "  ")(princ (car lst))(printnumbers (cdr lst) (1+ num) lar))
		(t (princ "    ")(princ (car lst)) (printnumbers (cdr lst) (1+ num) lar))
	
	)


)

(defun start()"start of the program"
(terpri)
(princ "choose board you want to play")
(terpri)
(princ "1.Play 2 * 2")
(terpri)
(princ "2.Play 5 * 5")
(terpri)
(setq ch(read))

(cond ((eq ch 1) (initializetwo)) ((eq ch 2)(initializefive)) (t (terpri) (princ "invalid choice") (start)))

)

(defun initializetwo()


)

(defun initializefive()"initializes five by five board"

(setq n 5)
(setq m 5)
(setq lst '((0 0 0 2 0)(2 3 2 0 2)(2 1 0 0 3)(0 1 2 0 3)(0 2 0 0 0)))
(setq counter(doublelist nil 5 5))
(setq lar(makelar 5 5 nil))
(setq uad (makeuad 5 5 nil))
(printboard 5 5 lst lar uad)
(playgame n m lar uad )
)

(defun setcommandvalue(x y command lar uad)

	(cond 
		((eq command 'r) (setf (nth (1+ y) (nth x lar) ) 'true ))
		((eq command 'l) (setf (nth y (nth x lar)) 'true))
		((eq command 'u) (setf (nth y (nth x uad)) 'true))
		((eq command 'u) (setf (nth (1+ y) (nth x uad)) 'true))
		(t (terpri) (princ "invalid value")(terpri))
	)

)


(defun playgame(n m lar uad)
(terpri)
(princ "enter command in the form (Number Number l,r,u,d):")
(terpri)
(setq ip(read-line ))
(setq ip (string-to-list ip))
(setq x (nth 0 ip))
(setq y (nth 1 ip))
(setq command (nth 2 ip))
(setcommandvalue x y command lar uad)


)

(defun readfromfile()
(setq in-file (open "input.dat" :direction :input))
(setq line(string-to-list (read-line in-file)))
(setq row (pop line))
(setq column (pop line))
(princ row)
(princ column)
(setq lst (listlinebyline in-file row column nil))
(close in-file)
lst
)


(defun listlinebyline(in-file m n lst)"reads input line by line and creates initial list"

(princ (read-line in-file))	
#|	(setq lst (list (string-to-list (read-line infile))))
	(
		loop for i from 1 to (1- n) do 
		(push ( string-to-list (read-line infile)) (cdr (last lst)))
	)

	lst
|#

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

(defun makelar(n m lst) "this creates double LAR list" 
	
	(cond
		((eq n 0) lst)
		(t (setq ls(makesinglelar (1+ m) nil)) (setq lst (append lst (list ls))) (makelar (1- n) m lst) )

	)

)
(defun makesinglelar(m lst)"this creates single LAR list"

	(cond
		((eq m 0) lst)
		((eq lst nil) (setq lst '(false)) (makesinglelar (1- m) lst) )
		(t (push 'false lst) (makesinglelar (1- m) lst) )
	

	)
)

(defun makeuad(n m lst) "this creates double UAD list"
	(
		cond 
			((eq n -1) lst)
			(t (setq ls(makesinglelar m nil))(setq lst (append lst (list ls))) (makeuad (1- n) m lst) )
	)
)


