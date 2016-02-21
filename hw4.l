;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PART 1, #1: CYCLE
;; reverses the order of items in the list
;; for each element, append car(l) to the end of cdr(l))

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
;; splits a list using the given atom as the delimitter


;; PART 1, #3: RANGE
;; given a list of numbers, returns smallest number, mean, and largest number
;(defun range(l)
;    (list min(l)(/total(l)(length l)
;)


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
;loop for a in l when (predicate) collect a


;; PART 1, #6: QUICKSORT
;; write quicksort for lisp; use PIVOT recursively to get a sorted list
