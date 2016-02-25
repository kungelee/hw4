;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PART 1, #1: CYCLE
;; cycle the items in the front to the back
(defun cycle (n l)
    (dotimes (var n)
        ; cdr returns the list, minus the first element
        (setq reversedL (cdr l))
            ;(format t "~%l:  ~a ~%" l)
        ; append to last
        (setq l (append reversedL (list (car l))))
            ;(format t "rL:  ~a ~%" reversedL)
            ;(format t "~%new l:  ~a ~%" l)
    )
    l
)

;; PART 1, #2: SPLIT-LIST
;; split a list using the given atom as the delimitter
(defun split-list (a l)    
    (setq currentL '())
    (setq finalL '())

    (loop for e in l do   
        ;;(format t "e = ~s ~%" e)
        ;;if
        (cond ( (eq e a)
                   ;;(format t "e == a; appending cL to fL ~%")
            (cond ( (not (eq currentL '()))      
                    ;;append current list to final list
                    ;;(format t "appending currentL to finalL ~%")
                    (cond 
                        ((eq finalL '() ) (push currentL finalL))
                        (t (push currentL (cdr (last finalL))))
                    )
                   ;;clear current list              
                   ;;(format t "clearing currentL ~%")
                   (setq currentL '())
                )
              )
             )
                
                ;;else
                (t
                    (cond
                            ((eq currentL '() )
                                ;;( format t "setting cL to e ~%" )
                                (push e currentL)
                            )
                        ;;append element to current list
                        ( t
                           ;;(format t "appending e to cL ~%")
                           (push e (cdr (last currentL)))                  
                        )
                    )   
                )
            )
        )

    (cond (
        (not (eq currentL '()))
        (cond 
                ( (eq finalL '() )
                     (push currentL finalL)
                )
                (t 
                    (push currentL (cdr (last finalL)))
                )
        )                  
        )
    )    
    finalL
)
    

;; PART 1, #3: RANGE
;; returns smallest, mean, and largest number in the list
(defun range (l)
    ;; smallest
    (if (/= (length l) 0)
        (setq minL (apply 'min l))
        (setq minL nil)
    )
    
    ;; mean: add all elements together, then divide by length
    (setq x (apply '+ l)) ; x holds sum of all elements
    (if (/= (length l) 0)
        (setq medianL (/ x (length l)))
    )
    
    ;; largest
    (if (/= (length l) 0)
        (setq maxL (apply 'max l))
    )
    
    (if (eq (length l) 0)
        (format nil "(~a)" nil)
        (format nil "(~a ~a ~a)" minL medianL maxL)
    )
)

;; PART 1, #4: SEARCH-COUNT
;; returns the number of times an atom occurs in a list
(defun search-count (a l)
    (setf count 0)
    (loop for e in l do
        (cond
            ((eq e a) (setf count (+ 1 count)))
        )
    )
    count
)

;; PART 1, #5: PIVOT
;; takes a list l and an int n; splits it so that one list has all int < n 
;; and the other list has all int > n
(defun pivot(n l)
    (if (null l) nil)
    (setq lessThanN '())
    (setq greaterThanN '())
    (dotimes (var (length l))
        (if (< (nth var l) n)
            (setq lessThanN (append lessThanN (list (nth var l)))) ;then
            (setq greaterThanN (append greaterThanN (list (nth var l)))) ;else
        )
    )
    (list lessThanN greaterThanN)
)

;; PART 1, #6: QUICKSORT
;; write quicksort for lisp; use PIVOT recursively to get a sorted list

(defun quicksort(l)
    (cond
        ((null l) 
            nil
        )
        (t
            (append
                (quicksort ( car (pivot (car l) (cdr l)) ))
                (cons (car l) nil)
                (quicksort ( cdr ( pivot (car l) (cdr l) ) ))
            )
        )
    )
)

;; PART 2, MATCH
;; shout-out to the TA + LISP slides for giving the pseudo code
(defun match(l1 l2)
    (cond 
        ((or (null l1) (null l2)) (equal l1 l2))
        ((eq (car l1) '?)
            (match (cdr l1) (cdr l2))
        )
        ((and (eq (car l1) '!) (match (cdr l1) (cdr l2)) (format nil "T")))
        ((eq (car l1) '!) (match l1 (cdr l2)))
        ((eq (car l1) (car l2)) (match (cdr l1) (cdr l2)))
    )
)

;; PART 3, #1: MATRIX-ADD
(defun matrix-add(m1 m2)
    ; LOOK AT THIS ONE LINE OMG !!
    (setq sum (mapcar (lambda (row1 row2) (mapcar '+ row1 row2)) m1 m2))
    ; holy crap that lecture was so helpful ??? ?? !! i see the beauty in lisp now lol
    ; still won't ever use it again tho
    
    (if (eq (length m1) 0)
        (format nil "~a" nil) ; then, there is an error
        (format nil "~a" sum) ; else
    )
)

;; PART 3, #2: MATRIX-MULTIPLY

;;(defun dot-product(a b) (reduce #'+ (mapcar #'* a b)))
(defun dot-product (a b) 
    (loop for p in a as q in b 
          sum (* p q)))
  
(defun matrix-multiply (m1 m2)
    (cond 
        ((eq m1 nil) nil)
        ((eq m2 nil) nil)
        ;;transpose m2
        (t
        (setq trM2 (apply 'mapcar (lambda (&rest newList) (apply 'list newList)) m2))
        (setq multMatr '())
        
        (loop for r in m1 do
            (setq row '())
            ;;(format t "row is now ~a ~%" row)

            (loop for c in trM2 do
                (setf dP (dot-product r c) )
                ;;(format t "dot product is ~a ~%" dP)
                (cond
                    ((eq row nil)
                        (push dP row)
                    )
                    (t
                        (push dP (cdr (last row)))
                    )
                )
                ;;(append row (list dP))
                (format t "row is now ~a ~%" row)
            )
            (cond
                
                ((eq row '() )
                    (push row multMatr)
                )
                ( t
                    (push row (cdr (last multMatr)))                  
                )
            )
            ;;(format t "just pushed row ~a to multmatr ~%" row)
        )
        multMatr
        
        
        )
    )
    
)

;; PART 3, #3: MATRIX-TRANSPOSE
(defun matrix-transpose(m1)
    ; shout-out to today's lecture for being so helpful w0w
    (setq flippedM1 (apply 'mapcar (lambda (&rest newList) (apply 'list newList)) m1))
    
    (if (eq (length m1) 0)
        (format nil "~a" nil) ; then, there is an error
        (format nil "~a" flippedM1) ; else
    )
)
