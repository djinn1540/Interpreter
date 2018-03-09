;Tommy Lu/Vishal Patel/Jamie Flynn
;3/8/2018
;1:24 AM implemented break/continue as call/cc functions, test 8-11 test 13-14 pass, test 12 may or may not pass, need to investigate error, separate d if and while statements in state'

;Import Parser
(require "simpleparser.scm")

;Interpret takes in a textfile and returns the value/bool mapped to 'return as the result of the returned m_state of the parsetree
;Start
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (m_state (parser filename) emptystate basicfunction basicfunction return basicfunction)))))

(define basicfunction (lambda (v) (error "usage from main")))
;The empty state is defined as a list of two empty lists
(define emptystate '(()()))

#|

Abstraction and helper methods such as member?
Relevant abstractions are left below the functions that have them, otherwise general ones can be found here

|#
  
;valuesoflayer gets the value list from a state while maintaining later layers
(define valuesoflayer cadr)
;Varsof gets the var list from a state
(define varsoflayer car)
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
(define boolop '(== != >= <= > < || && !))
;List of while operators
(define whileop '(while))
;List of if operators
(define ifop '(if))

#|

m_state should return a state

|#

;m_state parses each statement and runs them through functions and states and handles returning the state to the interpreter
(define m_state
  (lambda (parsedtree s break continue return throw)
    (cond
      ((null? parsedtree) s)
      ((eq? (stmttype parsedtree) 'return) (m_return (car parsedtree) s return))
      ((eq? (stmttype parsedtree) 'begin) (m_state (cdr parsedtree) (removelayer (m_state (cdar parsedtree) (addlayer s) break continue return throw)) break continue return throw))
      ((eq? (stmttype parsedtree) 'continue) (continue s))
      ((eq? (stmttype parsedtree) 'break) (break s))
      ((member? (stmttype parsedtree) declop) (m_state (cdr parsedtree) (s_declassign (car parsedtree) s) break continue return throw))
      ((member? (stmttype parsedtree) whileop) (m_state (cdr parsedtree) (removelayer (call/cc (lambda (break) (m_state_while (car parsedtree) (addlayer s) break continue return throw)))) break continue return throw))
      ((member? (stmttype parsedtree) ifop) (m_state (cdr parsedtree) (removelayer (m_state_if (car parsedtree) (addlayer s) break continue return throw)) break continue return throw)))))

(define blockparsedtree cdar)

(define
  (lambda (s)
    (append '(()()) s)))

(define removelayer cddr)


;Assigns 'return variable to a value/boolean in state and then returns the state
(define m_return
  (lambda (expr s return)
    (cond
      ((and (pair? (firstarg expr)) (member? (prefix (firstarg expr)) arithop)) (return (m_value (firstarg expr) s)))
      ((and (pair? (firstarg expr)) (member? (prefix (firstarg expr)) boolop)) (return (returntruefalse (m_bool (firstarg expr) s))))
      ((number? (firstarg expr)) (return (firstarg expr)))
      ((eq? 'true (firstarg expr)) (return (firstarg expr)))
      ((eq? 'false (firstarg expr)) (return (firstarg expr)))
      (else (return (s_find (firstarg expr) s))))))

(define returntruefalse
  (lambda (boolean)
    (if boolean
        'true
        'false)))
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
      ((and (eq? '- (operator expr)) (null? (operand2? expr))) (- 0 (m_value (operand1 expr) s)))
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
;The operand2? checks if there is a second argument to the unary operator -
(define operand2? cddr)
;The operator is the operator
(define operator car)
;The condition in an if or while statement
(define execcond cadr)
;The body of an if or while statement
(define body (lambda (lis) (cons (caddr lis) '())))
;The else clause of an if statment
(define elsebody (lambda (lis) (cdddr lis)))

;Checks what form of syntax between ifelse and if
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
(define m_state_while
  (lambda (expr s break continue return throw)
    (cond
      ((eq? (operator expr) 'while) (if (m_bool (execcond expr) s) (m_state_while expr (call/cc (lambda (continue) (m_state (body expr) s break continue return throw))) break continue return throw) s))
      (else (break s))))); return 

(define m_state_if
  (lambda (expr s break continue return throw)
    (cond
      ((isIfElse expr) (if (m_bool (execcond expr) s) (m_state (body expr) s break continue return throw) (m_state (elsebody expr) s break continue return throw)))
      ((isIf expr) (if (m_bool (execcond expr) s) (m_state (body expr) s break continue return throw) s)))))
#|

Functions that should return an updated state

|#

;Declare and/or assign a value by mapping a variable and value to the state
;1: for attempts to apply var to the same variable error 2: for attempts to simply declare a variable, we map it to the null list '() and add to state
;3: Declare a variable and determine if it is initalized to a boolean expression 4: Declare a variable and determine if it is initalized to a boolean
;5: for attempts to declare and assign at the same time we take both arguments 6: Error if variable is undeclared and an attempt to assign is done
;7: for attempts to reassign a variable to a boolean expression 8: for attempts to reassign a variable to a value expression 5: should not make it here error
(define s_declassign
  (lambda (expr s)
    (cond
      ((and (eq? (prefix expr) 'var) (s_isinstate? (firstarg expr) s)) (error 'cannotdeclaretwice))
      ((and (eq? (prefix expr) 'var) (null? (secondarg? expr))) (s_add (firstarg expr) uninitializedvalue s))
      ((and (eq? (prefix expr) 'var) (pair? (secondarg expr)) (member? (prefix (secondarg expr)) boolop)) (s_add (firstarg expr) (m_bool (secondarg expr) s) s))
      ((and (eq? (prefix expr) 'var) (or (eq? (secondarg expr) 'true) (eq? (secondarg expr) 'false))) (s_add (firstarg expr) (m_bool (secondarg expr) s) s))
      ((eq? (prefix expr) 'var) (s_add (firstarg expr) (m_value (secondarg expr) s) s))
      ((and (eq? (prefix expr) '=) (not (s_isinstate? (firstarg expr) s))) (error 'attemptingtoassigntoundeclared))
      ((and (eq? (prefix expr) '=) (pair? (secondarg expr)) (member? (prefix (secondarg expr)) boolop)) (s_add (firstarg expr) (m_bool (secondarg expr) s) s))
      ((and (eq? (prefix expr) '=) (s_isinstate? (firstarg expr) s)) (s_add (firstarg expr) (m_value (secondarg expr) s) s))
      (else (error 'unexpectederror)))))

;s_layer_add adds the var to the current layer of the state
(define s_layer_add
  (lambda (var val s)
    (cond
      (cons (s_add var val (car s)) (cdr s))))) ; state looks like (((local vars) (local vals))((global vars)(global vals)))

;Helper function that adds a variable to the state, possibly with a value. Also handles updating by removing a variable if it is already in the state and then reading it.
(define s_add
  (lambda (var val s)
    (cond
<<<<<<< HEAD
      ((number? val) (cons (cons var (varsof s)) (list (cons val (valuesof s)))))
      ((eq? #t val) (cons (cons var (varsof s)) (list (cons 'true (valuesof s)))))
      ((eq? #f val) (cons (cons var (varsof s)) (list (cons 'false (valuesof s)))))
      ((eq? 'true val) (cons (cons var (varsof s)) (list (cons 'true (valuesof s)))))
      ((eq? 'false val) (cons (cons var (varsof s)) (list (cons 'false (valuesof s)))))
      (else (cons (cons var (varsof s)) (list (cons (m_value val s) (valuesof s))))))))

;Removes variable and its associated value
=======
      ((s_isinstate? var s) (s_remove var val s))
      ((number? val) (append (cons (cons var (varsoflayer s)) (list (cons val (valuesoflayer s)))) (nextlayer s)))
      ((eq? #t val) (append (cons (cons var (varsoflayer s)) (list (cons 'true (valuesoflayer s)))) (nextlayer s)))
      ((eq? #f val) (append (cons (cons var (varsoflayer s)) (list (cons 'false (valuesoflayer s)))) (nextlayer s)))
      ((eq? 'true val) (append (cons (cons var (varsoflayer s)) (list (cons 'true (valuesoflayer s)))) (nextlayer s)))
      ((eq? 'false val) (append (cons (cons var (varsoflayer s)) (list (cons 'false (valuesoflayer s)))) (nextlayer s)))
      (else (append (cons (cons var (varsoflayer s)) (list (cons (m_value val s) (valuesoflayer s)))) (nextlayer s))))))

;Removes variable and its associated value then adds the var and new value associated with it to the earliest layer (most inner layer) it was found.
>>>>>>> tom
(define s_remove
  (lambda (var val s)
    (cond
      ((not (s_isinstate? var s)) (error 'varnotfound))
      ((not (member? var (varsoflayer s))) (append (cons (varsoflayer s) (list (valuesoflayer s))) (s_remove var val (nextlayer s))))
      ((eq? var (currentvar s)) (cons (cons var (restofvars s)) (list (cons val (restofvalues s)))))
      (else (cons (cons (currentvar s) (varsoflayer (s_remove var val (cons (restofvars s) (list (restofvalues s))))))
                        (list (cons (currentvalue s) (valuesoflayer (s_remove var val (cons (restofvars s) (list (restofvalues s))))))))))))


;find finds a value that is paired to a variable in state which is a (list list)
;1: state is empty 2: if this layer does not contain it and there is another layer, check the next one
;3: var list is empty  4: check if value is uninitalized 5: check first variable in var list, return value if it is the same 5: iterate through both lists of state and check again
(define s_find
  (lambda (var s)
    (cond
      ((null? s) (error 'stateisempty))
      ((not (s_isinstate? var s)) (error 'varnotinstate))
      ((and (null? (varsoflayer s)) (pair? (nextlayer s))) (s_find var (nextlayer s)))
      ((null? (varsoflayer s)) '())
      ((and (eq? (currentvar s) var) (null? (currentvalue s))) (error 'valueisuninitalized))
      ((eq? (currentvar s) var) (currentvalue s))
      (else (s_find var (append (cons (restofvars s) (list (restofvalues s))) (nextlayer s)))))))

;Get next layer, outside of the current layer
(define nextlayer cddr)
;Get the current variable to be looked at in the state
(define currentvar caar)
;Get the current value, that is mapped to current variable, to be looked at in the state
(define currentvalue caadr)

#|

Functions that should return a boolean

|#

;checks if the variable is declared and in the state, in any layer
;checks if a variable is in var list
(define s_isinstate?
  (lambda (var s)
    (cond
      ((member? var (varsoflayer s)) #t)
      ((pair? (nextlayer s)) (s_isinstate? var (nextlayer s)))
      (else #f))))

#|

Boolean function that returns booleans and helper function

|#

;Checks for boolean and boolean expressions and evaluates them
(define m_bool
  (lambda (condition s)
    (cond
      ((null? condition) (error 'invalidarguments))
      ((eq? condition 'true) #t)
      ((eq? condition 'false) #f)
      ((s_isinstate? condition s) (s_find condition s))
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

;Evaluates comparison operators
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

;&& case for m_bool function
(define and_and
  (lambda (expr s)
    (if (not(null? (cddr expr)))
        (and (m_bool (operand1 expr) s) (m_bool (operand2 expr) s))
        (error 'invalidsyntax))))
        
;|| case for m_bool function
(define or_or
  (lambda (expr s)
    (if (not (null? (cddr expr)))
        (or (m_bool (operand1 expr) s) (m_bool (operand2 expr) s))
        (error 'invalidsyntax))))
        
        
;checks if the given expression is an arithmetic expression
(define check_arithmetic
  (lambda (expr)
    (if (or (eq? (operator expr) '*) (eq? (operator expr) '+) (eq? (operator expr) '-) (eq? (operator expr) '/) (eq? (operator expr) '%))
        #t
        #f)))






;This is a simple way to check the parse tree of a textname ONLY FOR TESTING PURPOSES
;function not implemented however, we can check for example a text file called "example.txt"
;Included files are simpleparser.scm, lex.scm, this.rkt, and example.txt, three lines were uncommented in lex, two lines were uncommented in simpleparser, one line was commented in lex from the original sources
(define check
  (lambda (filename)
    (parser filename)))
