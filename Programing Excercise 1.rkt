; Jamie Flynn gjf20
;this is corrected in some places

;insert takes a number and inserts it into an ordered list

(define insert
  (lambda (x lis)
    (cond
      ((null? lis) x)
      ((> (car lis) x) (cons x lis))
      (else (cons (car lis) (insert x (cdr lis)))))))

;merge creates one in order list that contains the elements of two given ordered lists
(define merge
  (lambda (lis1 lis2)
    (cond
      ((null? lis1) lis2)
      ((null? lis2) lis1)
      ((< (car lis1) (car lis2)) (cons (car lis1) (merge (cdr lis1) lis2)))
      (else (cons (car lis2) (merge lis1 (cdr lis2)))))))

;removedups removes duplicates of an element that are consecutive (such that a single instance of the element remains)

(define removedups
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) lis)
      ((eq? (car lis) (car (cdr lis))) (cons (car lis) (removedups (cdr (cdr lis)))))
      (else (cons (car lis) (removedups (cdr lis)))))))

;split recurs to the end of a list and returns a pair of lists where the first has odd-th elements and the second has the even-th elements

(define split
  (lambda (lis)
    (cond
      ((null? lis) '(() ()))
      ((null? (cdr lis)) (cons (cons (car lis) '()) '(())))
      (else (cons (cons (car lis) (car (split (cdr (cdr lis))))) (cons (cons (car (cdr lis)) (car (cdr (split (cdr (cdr lis)))))) '()))))))

;deepcons inserts an atom directly to the left of the leftmost atom

(define deepcons
  (lambda (x lis)
    (cond
      ((list? (car lis)) (cons (deepcons x (car lis)) (cdr lis)))
      (else (cons x lis)))))

;numparens uses the list? function to count the numbers of parenthesis

(define numparens
  (lambda (lis)
    (cond
     ((null? lis) 1)
     ((pair? lis) (+ (numparens (car lis)) (numparens (cdr lis))))
     (else 0))))

;dup inserts a copy of each element in a list directly after that element
(define dup*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (dup* (car lis)) (cons (dup* (car lis)) (dup* (cdr lis)))))
      (else (cons (car lis) (cons (car lis) (dup* (cdr lis))))))))

;removedups* removes elements in a list that are the same as the element that preceeds it, does not operate across list boundaries
(define removedups*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (removedups* (car lis)) (removedups* (cdr lis))))
      ((null? (cdr lis)) lis)
      ((eq? (car lis) (car (cdr lis))) (removedups* (cdr lis)))
      (else (cons (car lis) (removedups* (cdr lis)))))))

;split* separates the even and odd index elements and recurs to separate elements within lists

(define split*
  (lambda (lis)
    (cond
      ((null? lis) '(() ()))
      ((and (null? (cdr lis)) (list? (car l))) (list (cons (split* (car lis)) (cadr (split* (cdr lis))))(cadr (split* (cdr lis)))))
      ((null? (cdr lis)) (list (cons (car lis) (car (split* (cdr lis))) (cadr (split* (cdr lis))))))
      ((list? (car lis)) (cons (cons (split (car lis)) '()) (cons (cdr (split (cdr lis))) '())))
      (else (cons (cons (car lis) (cons (car (split (cdr (cdr lis)))) '())) (cons (cons (car (cdr lis)) (cons (car (cdr (split (cdr (cdr lis))))) '())) '()))))))

; removedups** removes all duplicats including ducplicate lists


(define removedups**
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((and (list? (car lis)) (null? (cdr lis))) (cons (removedups** (car lis)) '()))
      ((and (list? (car lis)) (list? (cadr lis)) (equal? (removedups** (car lis)) (removedups** (cadr lis)))) removedups** (cdr lis))
      ((list? (car lis)) (cons (removedups** (car lis)) (removedups** (cdr lis))))
      ((null? (cdr lis)) lis)
      ((eq? (car lis) (cadr lis)) (removedups** (cdr lis)))
      (else (cons (car lis) (removedups** (cdr lis)))))))