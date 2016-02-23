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


;; PART 1, #3: RANGE
;; returns smallest, mean, and largest number in the list
(defun range (l)
    ;; smallest
    (if (/= (length l) 0)
        (setq minL (apply 'min l)) ;then statement
        (setq minL nil) ;else statement
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
;int search-count(e, l)
;    if e equals car(l)
;        return 1 + search-count(e, cdr(l))
;    else
;        return search-count(e, cdr(l))
;add terminating condition
;add null check

;; PART 1, #5: PIVOT
;; takes a list l and an int n; splits it so that one list has all int < n 
;; and the other list has all int > n
(defun pivot(n l)
    (setq lessThanN '())
    (setq greaterThanN '())
    (dotimes (var (length l))
        (if (< (nth var l) n)
            (setq lessThanN (append lessThanN (list (nth var l)))) ;then
            (setq greaterThanN (append greaterThanN (list (nth var l)))) ;else
        )
    )
    (if (/= (length l) 0)
        (format nil "(~a ~a)" lessThanN greaterThanN)
        (format nil "(~a ~a)" nil nil)
    )
)

;; PART 1, #6: QUICKSORT
;; write quicksort for lisp; use PIVOT recursively to get a sorted list

;; PART 2, MATCH
;; shout-out to the TA + LISP slides for giving the pseudo code
;(defun match(l1 l2)
;    (cond 
;        ((eq l1 nil) (format nil "T"))
;        ((char-equal (car l1) ?)
;            (match(rest l1) (rest l2))
;        )
;        ((char-equal (car l1) !)
;            (if (match(rest l1) (rest l2))
;                (format nil "T")
;            )
;        )
;        ((char-equal (car l1) !) (match l1 (rest l2)))
;        (t match(rest l1) (rest l2))
;    )
;)

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
(defun matrix-multiply (m1 m2)
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









