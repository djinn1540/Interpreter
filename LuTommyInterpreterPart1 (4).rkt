;Tommy Lu/Vishal Patel/Jaime Flynn
;2/19/18

;Import Parser
(require "simpleparser.scm")

;Interpret takes in a textfile and returns the result
;Start
(define interpret
  (lambda (filename)
    (m_state (parser filename) emptystate)))

;The empty state is defined as a list of two empty lists
(define emptystate '(()()))

#|

Abstraction and helper methods such as member?
Relevant abstractions are left below the functions that have them, otherwise general ones can be found here

|#
  
;Valuesof gets the value list from a state
(define valuesof cadr)
;Varsof gets the var list from a state
(define varsof car)
;Gets the cdr of the variable list, the rest of the variables in a state
(define restofvars cdar)
;Gets the cdr of the value list, the rest of the values in a state
(define restofvalues cdadr)
;Member checks for if an atom is a part of the list
(define member?
  (lambda (a lis)
    (cond
      ((null? lis) #f)
      ((eq? a (car lis)) #t)
      (else (member? a (cdr lis))))))
;For variables declared but not initalized, the null list is mapped with it
(define uninitializedvalue '())
;Get the first argument to a syntax rule
(define firstarg cadr)
;Get the second argument to a syntax rule
(define secondarg caddr)
;Check if there is a second argument to a syntax rule
(define secondarg? cddr)
;Check the prefix from the parsetree standpoint to know what to apply to the statement
(define stmttype caar)
;Check this particular expressions' prefix/operator
(define prefix car)

;List of arithmatic operators
(define arithop '(+ - / * %))
;List of declare and assign operators
(define declop '(var =))
;List of boolean operators
(define boolop '(== != >= <= > <))
;List of if and while operators
(define ifwhileop '(if while))

(define remaining_lines cdr)
(define current_line car) ;for the parse tree

#|

m_state should return booleans or values to the interpret

|#

;m_state parses each statement and runs them through functions and states and handles returning something to the interpreter
;1: end of parsedtree was reached but no return was found 2: return was found, so we return the value of the expression after return 3: operator is either a var or =, go to declaring and assigning function
(define m_state
  (lambda (parsedtree s)
    (cond
      ((null? parsedtree) s)
      ;((list? (prefix parsedtree)) (m_state (remaining_lines parsedtree) (m_state (current_line parsedtree) s)))
      ((eq? (stmttype parsedtree) 'return) (m_return (car parsedtree) s))
      ((member? (stmttype parsedtree) declop) (m_state (cdr parsedtree) (s_declassign (car parsedtree) s)))
      ((member? (stmttype parsedtree) ifwhileop) (m_state (cdr parsedtree) (m_state_ifwhile (car parsedtree) s))))))


;Return the argument that comes after return
;TODO handling booleans
(define m_return
  (lambda (expr s)
    (cond
      ((null? expr) '())
      ((number? expr) expr)
      ((boolean? expr) expr)
      ((and (pair? expr) (member? (prefix (firstarg expr)) arithop)) (m_value expr s))
      ((and (pair? expr) (member? (prefix (firstarg expr)) boolop)) (m_bool expr s))
      (else (m_value (firstarg expr) s)))))

#|

Functions that should return values

|#

;m_value returns the value of a variable, value, etc.
;In order by condition
;1: expression was empty, generally when there is a value and it is uninitalized to '() error 2: if value is number then return number 3: if it is variable then return variable value 4: if it is variable but variable is undeclared then
;error 5: unary -, flips the sign error 6-10: operator math 11: should not make it here error
(define m_value
  (lambda (expr s)
    (cond
      ((null? expr) expr)
      ((number? expr) expr)
      ((and (not (pair? expr)) (s_isinstate? expr s)) (s_find expr s))
      ((and (not (pair? expr)) (not (s_isinstate? expr s))) (error 'attemptingtooperateonundeclared))
      ((and (eq? '- (operator expr)) (not (secondary? expr))) (- 0 (m_value (operand1 expr) s)))
      ((eq? '+ (operator expr)) (+ (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
      ((eq? '- (operator expr)) (- (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
      ((eq? '* (operator expr)) (* (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
      ((eq? '/ (operator expr)) (quotient (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
      ((eq? '% (operator expr)) (remainder (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
      (else (error 'error)))))

;The operand1 is the first argument
(define operand1 cadr)
;The operand2 is the second argument
(define operand2 caddr)
;The operator is the operator
(define operator car)
;The condition in an if or while statement
(define execcond cadr)
;The body of an if or while statement
(define body (lambda (lis) (cons (caddr lis) '())))
;The else clause of an if statment
(define elsebody (lambda (lis) (car (cdddr lis))))

(define isIfElse
  (lambda (lis)
    (cond
      ((null? lis) #f)
      ((null? (operator lis)) #f)
      ((null? (execcond lis)) #f)
      ((null? (body lis)) #f)
      ((null? (elsebody lis)) #f)
      ((null? (cdr (cdddr lis))) #t)
      (else #f))))

(define isIf
  (lambda (lis)
    (cond
      ((null? lis) #f)
      ((null? (operator lis)) #f)
      ((null? (execcond lis)) #f)
      ((null? (body lis)) #f)
      ((null? (elsebody lis)) #t)
      (else #f))))
      

;takes an expression (list like '([if, while] (...) (...) (...)) and returns the state determined by the logic
(define m_state_ifwhile
  (lambda (expr s)
    (cond
      ((eq? (operator expr) 'while) (if (m_bool (execcond expr) s) (m_state_ifwhile expr (m_state (body expr) s)) s))
      ((isIfElse expr) (if (m_bool (execcond expr) s) (m_state (body expr) s) (m_state (elsebody expr) s)))    ; todo the and clause         todo also change the "variable-like" use of (define **** '())
      ((isIf expr) ; do it more with just the if body
      (else s))))); return 

#|

Functions that should return an updated state

|#

;Declare and/or assign a value by mapping a variable and value to the state
;1: for attempts to apply var to the same variable error 2: for attempts to simply declare a variable, we map it to the null list '() and add to state 3: for attempts to declare and assign at the same time we take both arguments
;4: for attempts to reassign a variable we take both arguments and equal 5: should not make it here error
(define s_declassign
  (lambda (expr s)
    (cond
      ((and (eq? (prefix expr) 'var) (s_isinstate? (firstarg expr) s)) (error 'cannotdeclaretwice))
      ((and (eq? (prefix expr) 'var) (null? (secondarg? expr))) (s_add (firstarg expr) uninitializedvalue s))
      ((eq? (prefix expr) 'var) (s_add (firstarg expr) (m_value (secondarg expr) s) s))
      ((and (eq? (prefix expr) '=) (not (s_isinstate? (firstarg expr) s))) (error 'attemptingtoassigntoundeclared))
      ((and (eq? (prefix expr) '=) (s_isinstate? (firstarg expr) s)) (s_add (firstarg expr) (m_value (secondarg expr) s) s))
      (else (error 'unexpectederror)))))

;Helper function that adds a variable to the state, possibly with a value. Also handles updating by removing a variable if it is already in the state and then reading it.
(define s_add
  (lambda (var val s)
    (cond
      ((s_isinstate? var s) (s_add var val (s_remove var s)))
      ((number? val) (cons (cons var (varsof s)) (list (cons val (valuesof s)))))
      (else (cons (cons var (varsof s)) (list (cons (m_value val s) (valuesof s))))))))

;Removes variable and its associated value
(define s_remove
  (lambda (var s)
    (cond
      ((not (s_isinstate? var s)) (error 'varnotfound))
      ((eq? var (currentvar s)) (cons (restofvars s) (list (restofvalues s))))
      (else (cons (cons (currentvar s)) (car (s_remove var (cons (restofvars s)) (list (restofvalues s))))) (list (cons (restofvalues s)) (cadr (s_remove var (cons (restofvalues s)) (list (restofvalues s))))))))))

;find finds a value that is paired to a variable in state which is a (list list)
;1: state is empty 2: var list is empty 3: check first variable in var list, return value if it is the same 4: iterate through both lists of state and check again
(define s_find
  (lambda (var s)
    (cond
      ((null? s) (error 'stateisnottwolists))
      ((null? (varsof s)) (error 'varlistisempty))
      ((and (eq? (currentvar s) var) (null? (currentvalue s))) (error 'valueisuninitalized))
      ((eq? (currentvar s) var) (currentvalue s))
      (else (s_find var (cons (restofvars s)) (list (restofvalues s)))))))

;Get the current variable to be looked at in the state
(define currentvar caar)
;Get the current value, that is mapped to current variable, to be looked at in the state
(define currentvalue caadr)

#|

Functions that should return a boolean

|#

;checks if the variable is declared and in the state
;checks if a variable is in var list
(define s_isinstate?
  (lambda (var s)
    (if (member? var (varsof s))
        #t
        #f)))

#|
Consolidated
|#


(define m_bool
  (lambda (condition s)
    (cond
      ((null? condition) (error 'invalidarguments))
      ((not (list? condition)) ;this is if just an atom is passed in
       (if (boolean? condition);checks if the atom is a boolean if so return it
           condition
           (error 'unexpectederror);error
        ))
      ;not_op: check the not case
      ((eq? (operator condition) '!) (not (m_bool(operand1 condition) s)))
      ;and case
      ((eq? (operator condition) '&&) (and_and condition s))
      ;or case
      ((eq? (operator condition) '|| )(or_or condition s))
      ;first two comparison operators that can be applied to non-numbers
      ((eq? '== (operator condition)) (equals_equals condition s))
      ;not equals
      ((eq? '!= (operator condition)) (not_equals condition s))
      ;Arithmetic comparisons
      ((eq? '>= (operator condition)) (comparison_op condition s)) 
      ((eq? '<= (operator condition)) (comparison_op condition s))
      ((eq? '> (operator condition)) (comparison_op condition s))
      ((eq? '< (operator condition)) (comparison_op condition s))
      (else (error 'unexpectederror)))))

(define comparison_op
  (lambda (expr s)
    ;comparison operators continued
    (cond 
          ((null? (secondarg expr)) #f)
          ;use m_value in case a list with arithmetic operators are given
          ((eq? '>= (operator expr)) (>= (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
          ((eq? '<= (operator expr)) (<= (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
          ((eq? '> (operator expr)) (> (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
          ((eq? '< (operator expr)) (< (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
          (else #f))))


  ; == case for m_bool function
  (define equals_equals
    (lambda (expr s)
      (cond
        ;checks if operands need further evaluation 
        ((or(list? (operand1 expr)) (list? (operand2 expr)))
         (if (or(check_arithmetic (operand1 expr)) (check_arithmetic (operand2 expr)))
             (= (m_value (operand1 expr) s) (m_value (operand2 expr) s))
             (eqv? (m_bool(operand1 expr) s) (m_bool (operand2 expr) s))))
         (else (eq? (m_value (operand1 expr) s) (m_value (operand2 expr) s))))))
         
       
  ;!= case for m_bool function
  (define not_equals
    (lambda (expr s)
      (cond
        ((or(list? (operand1 expr)) (list? (operand2 expr)))
         (if (or(check_arithmetic (operand1 expr)) (check_arithmetic (operand2 expr)))
             (not (eqv? (m_value (operand1 expr) s) (m_value (operand2 expr) s)))
             (not (eq? (m_bool(operand1 expr) s) (m_bool(operand2 expr) s)))))
         (else (not(eq? (m_value (operand1 expr) s) (m_value (operand2 expr) s)))))))

(define and_and
  (lambda (expr s)
    (if (not(null? (cddr expr)))
        (and (m_bool (operand1 expr) s) (m_bool (operand2 expr) s))
        (error 'invalidsyntax))))
        
        
(define or_or
  (lambda (expr s)
    (if (not (null? (cddr expr)))
        (or (m_bool (operand1 expr) s) (m_bool (operand2 expr) s))
        (error 'invalidsyntax))))
        
        
;checks if the given expression is an arithmetic expression
(define check_arithmetic
  (lambda (expr)
    (if (or (eq? (car expr) '*) (eq? (car expr) '+) (eq? (car expr) '-) (eq? (car expr) '/))
        #t
        #f)))







;This is a simple way to check the parse tree of a textname ONLY FOR TESTING PURPOSES
;function not implemented however, we can check for example a text file called "example.txt"
;Included files are simpleparser.scm, lex.scm, this.rkt, and example.txt, three lines were uncommented in lex, two lines were uncommented in simpleparser, one line was commented in lex from the original sources
(define check
  (lambda (filename)
    (parser filename)))