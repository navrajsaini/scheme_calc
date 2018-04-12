#lang racket
;Inclusion stuff
(require rnrs/mutable-pairs-6)

;Predefined global variables for use in the program.
(define iValue "")
(define tList '())
(define varList '())
(define funcList '())
(define jFlag #f)
(define I 0) ;outer loop index
(define J 0) ;inner loop index
(define funcwrite (open-output-file "func.txt" #:exists 'can-update)) ;file for functions
(define varwrite (open-output-file "var.txt" #:exists 'can-update)) ;file for variables

;------------------------------------------------------------------------------------------------
(define flagif 0)
(set! flagif #f)
;-----------------^ needed ^



;(define (a)
;  (display (calculate eval)))
;(define eval (list  "(" 1 "+"  4 ")" "*" 6  "/" -2 "+" -7 "*" 2) )
;(define tokenizer (list "if" "(" 1 "<>" 0")"))
;(define d (list"(" 3 ">" 1")"))
;--------- tests^
;tests are above for the lower functions


;relational compares for if statments and the mid part of a for loop
(define (relational c)
  (cond
    [(equal? "==" (second c)) (cond
                            [(equal? (first c) (third c))
                             (set! flagif #t )])]  
    [(equal? "<>" (second c)) (cond
                            [(not (equal? (first c) (third c)))
                             (set! flagif #t )])] 
    [(equal? ">=" (second c)) (cond
                            [(or (> (first c) (third c)) (equal? (first c) (third c)))
                             (set! flagif #t )])]    
    [(equal? "<=" (second c)) (cond
                             [(or (< (first c) (third c)) (equal? (first c) (third c)))
                              (set! flagif #t )])] 
    [(equal? ">" (second c)) (cond
                            [(> (first c) (third c))
                             (set! flagif #t )])]    
    [(equal? "<" (second c)) (cond
                            [(< (first c) (third c))
                             (set! flagif #t )])]
    [else (set! flagif #f)]
    )
  ;(display flagif)(newline)
  )
; checks which is being called, if for else, or inside the then part
(define (check str) 
  (cond
    [(and (equal? #t flagif) (not (equal? "endif" (first str))))
     (cond
       [(equal? "=" (second str))
        (begin (varsub (rest (rest str))) (set! str c1) (calculate (rest(rest(str)))) (setVariable (first str) b1) (set! flagif "done" ) (display b1) )]
       [(equal? "output" (first str))
        (begin (if (checkVars (second str)) ;(display str)(newline)
                   (outputHandler (numStr str))
                   (begin (display (second str))(newline))
                   )(set! flagif "done" ))]
       [else (calculate str) (set! flagif "done" )])]
    [(equal? "if" (first str)) (relational (rest (rest str)))]
    [(and (equal? "elseif" (first str)) (not (equal? flagif "done" ))) (begin(set! flagif #t ) (relational (rest (rest str))) )] ;
    [(equal? "endif" (first str)) (set! flagif #f )]
    ))

;(set! flagif #t )
;breaks appart the list of lines to then be broken into tokens by tokenize
(define (break together)
  ;(display together)(newline)
  (let ((apart (list)))
   (for([i together])
   (set! apart (tokenize i))
    ;(display apart)(newline)
    (check apart)
    ;(check eval)
    (set! apart '())
   )
    )
  )
;-----------------------this is the variable substitution function. it works. still have to test the part that goes to andrew.

(define c1 (list))
;variable for substitution.
(define (varsub rep)
  (for([i rep])
    (cond
      [(equal? "I" i)(set! c1 (append c1 (list I)))]
      [(equal? "J" i)(set! c1 (append c1 (list J)))]
      [(equal? "(" i)(set! c1 (append c1 (list i)))]
      [(equal? ")" i)(set! c1 (append c1 (list i)))]
      [(equal? "==" i)(set! c1 (append c1 (list i)))]
      [(equal? "=" i)(set! c1 (append c1 (list i)))]
      [(equal? "<" i)(set! c1 (append c1 (list i)))]
      [(equal? ">" i)(set! c1 (append c1 (list i)))]
      [(equal? "<>" i)(set! c1 (append c1 (list i)))]
      [(equal? "<=" i)(set! c1 (append c1 (list i)))]
      [(equal? ">=" i)(set! c1 (append c1 (list i)))]
      [(equal? "+" i) (set! c1 (append c1 (list i)))]
      [(equal? "-" i)(set! c1 (append c1 (list i)))]
      [(equal? "*" i)(set! c1 (append c1 (list i)))]
      [(equal? "^" i)(set! c1 (append c1 (list i)))]
      [(equal? "/" i)(set! c1 (append c1 (list i)))]
      [(equal? "if" i)(set! c1 (append c1 (list i)))]
      [(equal? "for" i)(set! c1 (append c1 (list i)))]
      [(equal? "stepsize" i)(set! c1 (append c1 (list i)))]
      [(equal? "endfor" i)(set! c1 (append c1 (list i)))]
      [(equal? "endif" i)(set! c1 (append c1 (list i)))]
      [(number? i) (set! c1 (append c1 (list i)))]
      [else (set! c1 (append c1 (list (list-ref (getVariable i) 1))))]
      
)))


;----------------- the finnished calculate function with assistant functions.
(define a1 (list))
(define b1 (list'()))
(define (calculate eval)
  (let [(tot 0)]
    (cond
      [(empty? eval)
       ]
      [(and (equal? 1 (length eval))(equal? 1 (length b1) ))
       ]
      [(equal? 1 (length eval))
       (begin(set! a1 (append a1 (list eval))) (set! b1 a1) (set! a1 (list)) (calculate b1))]
      [(equal? 2 (length eval))
       (begin(set! a1 (append a1 eval)) (set! b1 a1) (set! a1 (list)) (calculate b1))]
      [(equal? 3 (length eval))
       (begin(set! a1 (append a1 (list(evaluate eval)))) (set! b1 a1) (set! a1 (list)) (calculate b1))]
      [(and (equal? 4 (length eval)) (equal? "("(third eval)))
       (begin(set! a1 (append a1 (list(evaluate (append (list(first eval)) (list(second eval)) (list (fourth eval))))))) (set! b1 a1) (set! a1 (list)) (calculate b1))]
      [(and (equal? "(" (first eval)) (equal? ")" (fifth eval)) (equal? 5 (length eval)))
       (begin (set! a1 (append a1 (list(evaluate (rest eval))))) (set! b1 a1) (set! a1 (list)) (calculate b1))]
      [(equal? ")" (second eval))
       (begin (set! a1 (append (list(first eval)) (list (second eval)))) (calculate(rest (rest eval))))]
      [(and(equal? "+" (second eval)) (not(equal? ")"(fourth eval))))
       (begin (set! a1 (append a1 (list(first eval))(list(second eval)))) (calculate (rest (rest eval))))]   
      [(and(equal? "-" (second eval) )(not(equal? ")"(fourth eval))))
       
       (begin (set! a1 (append a1 (list(first eval))(list(second eval)))) (calculate (rest (rest eval))))]
      [(and(equal? "*" (second eval)) (number? (third eval)) (or(equal? '() (fourth eval)) (not (equal? "^" (fourth eval))))) 
       (evaluate2 eval)]
      [(and(equal? "/" (second eval)) (number? (third eval)) (or(equal? '() (fourth eval))(not (equal? "^" (fourth eval)))))
       (evaluate2 eval)]
     
      [(and(equal? "(" (first eval)) (equal? ")"(fifth eval)) )
       (begin (calculate(append (list(evaluate (rest eval))) (rest(rest(rest(rest(rest eval))))))))]
      
      [(and(equal? "(" (first eval)) (equal? "(" (fourth eval)))
       (begin(set! a1 (append a1 (list(first eval)) (list(second eval)) (list(third eval))))
             (calculate (rest (rest (rest eval)))))]
      [(and(number? (first eval)) (equal? ")" (second eval)))
       (begin(set! a1 (append a1 (list(first eval)) (list(second eval)) (list(third eval))))
              (calculate (rest (rest (rest eval)))))]
      [(and(equal? "^" (second eval)) (number? (third eval)))
       (calculate (append (list(evaluate eval)) (rest (rest (rest eval)))))  ]
      [(and(equal? "^" (second eval)) (equal? "(" (third eval)) (equal? ")" (seventh eval)))
       (calculate(append (list(first eval)) (list(second eval)) (list(evaluate (rest (rest (rest eval)))))  (rest (rest (rest (rest (rest (rest (rest eval)))))))))]
      [(and(equal? "^" (second eval)) (equal? "^" (fourth eval)))
        (begin(set! a1 (append a1 (list(first eval)) (list(second eval)))) (calculate (rest(rest eval))))]
      [(equal? "^" (fourth eval))
       (begin (set! a1 (append a1 (list(first eval)) (list(second eval)))) (calculate (rest(rest eval))))]
      [(and(equal? "*" (second eval)) (equal? "("(third eval)))
       (begin(set! a1 (append a1 (list (first eval)) (list (second eval)))) (calculate (rest(rest eval))))]
      [(and(equal? "/" (second eval)) (equal? "("(third eval)))
       (begin(set! a1 (append a1 (list (first eval)) (list (second eval)))) (calculate (rest(rest eval))))]
  )
  ))
;these are for each type of evaluation.
(define (evaluate ev)
  (cond
    [(equal? "^" (second ev)) (expt (first ev) (third ev))]
    [(equal? "*" (second ev)) (* (first ev) (third ev))]
    [(equal? "/" (second ev)) (/ (first ev) (third ev))]
    [(equal? "+" (second ev)) (+ (first ev) (third ev))]
    [(equal? "-" (second ev)) (- (first ev) (third ev))]
    ))
(define (evaluate1 ev)
  (cond
    [(< 3 (length ev))  ev ]
    [(equal? 3 (length ev))  (evaluate ev) ]
    [(equal? "^" (second ev)) (exp (first ev) (evaluate1 (rest (rest ev))))]
    [(equal? "*" (second ev)) (* (first ev) (evaluate1 (rest (rest ev))))]
    [(equal? "/" (second ev)) (/ (first ev) (evaluate1 (rest (rest ev))))]
    [(equal? "+" (second ev)) (+ (first ev) (evaluate1 (rest (rest ev))))]
    [(equal? "-" (second ev)) (- (first ev) (evaluate1 (rest (rest ev))))]
    ))
(define (evaluate2 ev)
  (cond
    [(equal? "*" (second ev)) (calculate (append (list(* (first ev) (third ev))) (rest(rest (rest ev )))))]
    [(equal? "/" (second ev)) (calculate (append (list(/ (first ev) (third ev))) (rest(rest (rest ev )))))]
    ))



;####################################################################################################################

;Helper function that lets an append work as one would expect. Still does not work :(.
;(define (appendList aList bList)
;  (set! aList (append aList bList))
;  )

;Raj's function adding function
(define (add_func func)
  (for ([i func])
    (if (output-port? funcwrite)
        (begin
          (with-output-to-file "func.txt" #:exists 'append
            (lambda()
              (display i)
              (newline))))
        (begin (funcwrite (open-input-output-file "func.txt" #:exists 'append))
               (with-output-to-file "func.txt"
                 (lambda()
                   (display i)
                   (newline)))))))

;Raj's variable adding function
(define (add_var var)
  (if (output-port? varwrite)
  (begin
  (with-output-to-file "var.txt" #:exists 'append
    (lambda()
      (display var)
      (newline))))
  (begin (varwrite (open-input-output-file "var.txt" #:exists 'append))
  (with-output-to-file "var.txt"
    (lambda()
      (display var)
      (newline))))))

;Quick and dirty function to write the entirety of the varList to file.
(define (saveVars)
  ;(display varList)
  (if (output-port? varwrite)
  (begin
  (with-output-to-file "var.txt" #:exists 'replace
    (lambda()
      (for ([i varList])
      (display (string-join (list (list-ref i 0)(if (number? (list-ref i 1))
                                                    (number->string (list-ref i 1))
                                                    (list-ref i 1)
                                                    )(list-ref i 2))))
      (newline)))))
  (begin (varwrite (open-input-output-file "var.txt" #:exists 'replace))
  (with-output-to-file "var.txt"
    (lambda()
      (for ([i varList])
      (display (string-join (list (list-ref i 0)(if (number? (list-ref i 1))
                                                    (number->string (list-ref i 1))
                                                    (list-ref i 1)
                                                    )(list-ref i 2))))
      (newline))))))
  )

;clear function to delete the files and recreate em
; also empty the lists
(define clear
  (lambda(); all of these don't work in windows maybe better luck in linux?
    (close-output-port funcwrite)
    (delete-file "func.txt")
    ;create the file again
    (open-output-file "func.txt")
    (close-output-port varwrite)
    (delete-file "var.txt")
    ;create the file again
    (open-output-file "var.txt")
    (set! varList '())
    (set! funcList '())
    ))

;read the file to list var
;(for ([i list]) (toJordan i)) -> send jordan the list of vars
(define (read_var)
  (set! varList '())
  (let ((sub '()))
  (for ([i (file->lines "var.txt")])
    (set! sub (string-split i))
    (defineVariable (list (list-ref sub 0) (list-ref sub 2)))
    ;(display "Between def and set.\n")
    (setVariable (list-ref sub 0) (string->number (list-ref sub 1)))
    )))
       
;functions file to list
(define (read_func)
  (set! funcList '())
  (let ((sub '()))
  (for ([i (file->lines "func.txt")])
    (if (equal? "#definefunc" i)
        (begin (set! sub (append sub (list i)))
               (defineFunction sub)
               (set! sub '())
               )
    (begin (set! sub (append sub (list i)))
    )))))

;Takes the start of the if statement and then allows the user to
;keep adding any necessary expressions for completing the statement.
;Passes the list of tokens to a function that will handle the
;processing of the if statement.
(define (ifHandler tokenList)
  ;(display "If statement.\n")
  (set! iValue (read-line))
  (if (equal? iValue "endif")
      (begin (set! tokenList (append tokenList (list iValue)))(break tokenList))
      (begin (set! tokenList (append tokenList (list iValue)))(ifHandler tokenList))
      )
  )

;Runs the contents of a single loop iteration.
(define (loop tokenList)
  (let ((tokens '())
        (ifList '())
        (ifFlag #f))
    (for ([i tokenList])
      (set! tokens (string-split i))
      (cond
        [(equal? (list-ref tokens 0) "endfor") '()]
        [(equal? (list-ref tokens 0) "if") (set! ifFlag #t)(set! ifList (append ifList (string-join tokens)))]
        [(and (equal? (list-ref tokens 0) "endif") (not (false ifFlag))) (set! ifFlag #f)(set! ifList (append ifList (string-join tokens)))(break ifList)]
        [(equal? (list-ref tokens 0) "endif") (display "Unexpected endif.\n")]
        [(not (false? ifFlag)) (set! ifList (append ifList (string-join tokens)))]
        [(equal? (list-ref tokens 0) "input") (if (checkVars (list-ref tokens 1))
                                                  (inputHandler tokens)
                                                  (display "Can only input into global variables.\n")
                                                  )]
        [(equal? (list-ref tokens 0) "output") (outputHandler tokens)]
        [(equal? (list-ref tokens 0) "#definevari") (display "Cannot define variables within a function.\n")]
        [(equal? (list-ref tokens 0) "#definefunc") (display "Cannot define functions within a function.\n")]
        [(equal? (list-ref tokens 0) "#clear") (display "Cannot clear variables and functions within a function.\n")]
        [(checkVars (list-ref tokens 0)) (varHandler (tokenize (string-join tokens)))]
        [(checkFunc (list-ref tokens 0)) (funcHandler tokens)]
        [else (calculate (tokenize (string-join tokens)))]
        )
      (set! tokens '())
      )
    )
  )

;Runs the contents of a nested loop iteration, which will call a single loop iteration.
(define (innerLoop tokenList)
  (let ((tokens '())
        (ifList '())
        (ifFlag #f)
        (innerFlag #f)
        (innerFor '()))
    (for ([i tokenList])
      (set! tokens (string-split i))
      (cond
        [(and (equal? (list-ref tokens 0) "for") (not (false? innerFlag))) (display "Unexpected for.\n")]
        [(and (equal? (list-ref tokens 0) "for") (false? innerFlag)) (set! innerFlag #t)(set! innerFor (append innerFor (list (string-join (numStr tokens)))))]
        [(and (equal? (list-ref tokens 0) "endfor") (not (false? innerFlag))) (set! innerFlag #f) (set! innerFor (append innerFor (list (string-join (numStr tokens))))) (nestedFor innerFor)]
        [(equal? (list-ref tokens 0) "endfor") '()]
        [(not (false? innerFlag)) (set! innerFor (append innerFor (list (string-join (numStr tokens)))))]
        [(equal? (list-ref tokens 0) "if") (set! ifFlag #t)(set! ifList (append ifList (string-join tokens)))]
        [(and (equal? (list-ref tokens 0) "endif") (not (false ifFlag))) (set! ifFlag #f)(set! ifList (append ifList (string-join tokens)))(break ifList)]
        [(and (equal? (list-ref tokens 0) "endif") (false ifFlag)) (display "Unexpected endif.\n")]
        [(not (false? ifFlag)) (set! ifList (append ifList (string-join tokens)))]
        [(equal? (list-ref tokens 0) "input") (if (checkVars (list-ref tokens 1))
                                                  (inputHandler tokens)
                                                  (display "Can only input into global variables.\n")
                                                  )]
        [(equal? (list-ref tokens 0) "output") (outputHandler tokens)]
        [(equal? (list-ref tokens 0) "#definevari") (display "Cannot define variables within a loop.\n")]
        [(equal? (list-ref tokens 0) "#definefunc") (display "Cannot define functions within a loop.\n")]
        [(equal? (list-ref tokens 0) "#clear") (display "Cannot clear variables and functions within a loop.\n")]
        [(checkVars (list-ref tokens 0)) (varHandler (tokenize (string-join tokens)))]
        [(checkFunc (list-ref tokens 0)) (funcHandler tokens)] ;(display tokens)
        [else (calculate (tokenize (string-join tokens)))]
        )
      (set! tokens '())
      )
    )
  )

;Runs a single loop entry.
(define (singleLoopFor tokenList)
  (let ((init 0)
        (step 0)
        (max 0)
        (tokens '()))
    (set! tokens (string-split (car tokenList)))
    ;(display tokens)(newline)
    (set! init (string->number (list-ref tokens 3)))
    (set! step (string->number (list-ref tokens 7)))
    (set! max (string->number (list-ref tokens 5)))
    (for ([i (in-range init max step)])
      (set! I i)
      ;(display I)(newline)
      (loop (cdr tokenList))
      )
    )
  )

;Runs a single loop entry.
(define (nestedFor tokenList)
  (let ((init 0)
        (step 0)
        (max 0)
        (tokens '()))
    (set! tokens (string-split (car tokenList)))
    ;(display tokens)(newline)
    (set! init (string->number (list-ref tokens 3)))
    (set! step (string->number (list-ref tokens 7)))
    (set! max (string->number (list-ref tokens 5)))
    (for ([j (in-range init max step)])
      (set! J j)
      ;(display J)(newline)
      (loop (cdr tokenList))
      )
    )
  )

;Runs a nested loop entry.
(define (nestedLoopFor tokenList)
  (let ((init 0)
        (step 0)
        (max 0)
        (tokens '()))
    (set! tokens (string-split (car tokenList)))
    ;(display tokens)(newline)
    (set! init (string->number (list-ref tokens 3)))
    (set! step (string->number (list-ref tokens 7)))
    (set! max (string->number (list-ref tokens 5)))
    
    (for ([i (in-range init max step)])
      (set! I i)
      (innerLoop (cdr tokenList))
      )
    )
  )

;Checks if the tokenList contains more than one loop, then sends the user to one of functions accordingly.
(define (forProcessing tokenList)
  (if (equal? (length (indexes-of tokenList "endfor")) 2)
      (nestedLoopFor tokenList)
      (singleLoopFor tokenList)
      )
  )

;Takes the start of the for loop and then allows the user to keep
;adding any necessary expressions for completing the statement.
;Passes the list of tokens to a function that will handle the
;processing of the for statement.
(define (forHandler tokenList)
  (display "For statement.\n")
  (set! iValue (read-line))
  (cond [(and (equal? iValue "endfor") (not jFlag)) (begin (set! tokenList (append tokenList (list iValue)))(forProcessing tokenList))]
        [(and (equal? iValue "endfor") jFlag)(begin (set! tokenList (append tokenList (list iValue)))(set! jFlag #f)(forHandler tokenList))]
        [(and (equal? (list-ref (string-split iValue) 0) "for") (not jFlag))(begin (set! tokenList (append tokenList (list iValue)))(set! jFlag #t)(forHandler tokenList))]
        [(and (equal? (list-ref (string-split iValue) 0) "for") jFlag)(begin (display "Cannot nest loops more than once.\n")(forHandler tokenList))]
        [else (begin (set! tokenList (append tokenList (list iValue)))(forHandler tokenList))]
      )
  )

;Function checks to see if a passed variable name is a defined variable.
(define (checkVars varName)
  ;(display varName)(newline)
  (let ((bool #f))
  (if (not (zero? (length varList)))
      (for ([i varList])
        (if (equal? (list-ref i 0) varName)
            (set! bool #t)
            '()
            )
        )
      (set! bool #f)
      )
    bool
    )
  )

;Function checks to see if a passed function name is a defined function.
(define (checkFunc funcName)
  (let ((bool #f))
  (if (not (zero? (length funcList)))
      (for ([i funcList])
        (if (equal? (list-ref (string-split (list-ref i 0)) 1) funcName)
            (set! bool #t)
            '()
            )
        )
      (set! bool #f)
      )
    bool
    )
  )

;Sets a variable by finding the variable by name, then changing the value.
(define (setVariable varName value)
  (let ((var '()))
    ;(display varName)(display ": ")(display value)(display " :")(display (checkVars varName))(newline)
    (if (checkVars varName)
        (for ([i varList])
          ;(display varName)(display ": ")(display value)(newline)
          (if (equal? (list-ref i 0) varName)
              (cond [(equal? (list-ref i 2) "integer") (if (integer? value)
                                                          (set! varList (list-set varList (index-of varList i) (list (list-ref i 0) value (list-ref i 2))))
                                                          (begin (display "Value is not an integer.\n")) ;(display value)(newline))
                                                          )]
                    [(equal? (list-ref i 2) "float") (if (and (number? value) (not (exact-integer? value)))
                                                        (set! varList (list-set varList (index-of varList i) (list (list-ref i 0) value (list-ref i 2))))
                                                        (display "Value is not a float.\n")
                                                        )]
                    [(equal? (list-ref i 2) "boolean") (if (or (equal? value "true")(equal? value "false"))
                                                          (set! varList (list-set varList (index-of varList i) (list (list-ref i 0) value (list-ref i 2))))
                                                          (display "Value is not a boolean.\n")
                                                          )]
                    [else (display "Invalid data type.\n")]
                    )
              '()
              )
          )
        (display "Variable not yet defined.\n")
        )
    )
  )

;Gets a variable by first checking if it exists, then returning it from the list.
(define (getVariable varName)
  (let ((var '()))
    ;(display varName)(newline)
    (if (checkVars varName)
        (for ([i varList])
          (if (equal? (list-ref i 0) varName)
              (set! var i)
              '()
              )
          )
        (display "Variable not yet defined.\n")
        )
    var
      )
  )

;Checks if the input is correct, then if the variable exists. Then resolves it.
(define (inputHandler tokenList)
  ;(display "Input statement.\n")
  (if (equal? (length tokenList) 2)
      (if (checkVars (list-ref tokenList 1))
          (setVariable (list-ref tokenList 1) (string->number (read-line)))
          (display "Variable has not yet been defined.\n")
          )
      (display "Input error, please use: 'input <variable>'.\n")
      )
  )

;Checks if the input is correct, then if the variable exists. Then outputs it.
(define (outputHandler tokenList)
  ;(display "Output statement.\n")
  (if (equal? (length tokenList) 2)
      (if (checkVars (list-ref tokenList 1))
          (begin (display (list-ref (getVariable (list-ref tokenList 1)) 1)) (newline))
          (if (equal? (list-ref tokenList 1) "I")
              (begin (display I) (newline))
              (if (equal? (list-ref tokenList 1) "J")
                  (begin (display J) (newline))
                  (display "Variable has not yet been defined.\n")
                  )
              )
          )
      (if (equal? (length tokenList) 3)
          (begin (display (list-ref tokenList 1)) (display (list-ref (getVariable (list-ref tokenList 2)) 1)) (newline))
          (display "Output error, please use: 'output <variable>'.\n")
          )
      )
  )


;Defines a variable with the given name and the given values.
(define (defineVariable tokenList)
  (let ((int "integer")
        (float "float")
        (bool "boolean")
        (name (list-ref tokenList 0))
        (type (list-ref tokenList 1)))
    (cond [(equal? type int)(begin (set! varList (append varList (list (list name 0 type)))))] ;(add_var (string-join (list name (number->string 0) type)))
          [(equal? type float)(begin (set! varList (append varList (list (list name 0.0 type)))))] ;(add_var (string-join (list name (number->string 0.0) type)))
          [(equal? type bool)(begin (set! varList (append varList (list (list name false type)))))] ;(add_var (string-join (list name false type)))
          [else (display "Invalid data type.\n")]
  )))

;Checks if the #definevari command is correct or exists,
;then defines it.
(define (defVarHandler tokenList)
  ;(display "Define variable statement.\n")
  (if (and (equal? (length tokenList) 3) (or (equal? (list-ref tokenList 2) "integer")(equal? (list-ref tokenList 2) "float")(equal? (list-ref tokenList 2) "boolean")))
      (if (or (number? (list-ref tokenList 1))(equal? (list-ref tokenList 1) "I")(equal? (list-ref tokenList 1) "J")(equal? (list-ref tokenList 1) "+")(equal? (list-ref tokenList 1) "-")(equal? (list-ref tokenList 1) "*")(equal? (list-ref tokenList 1) "/")(equal? (list-ref tokenList 1) "^")(equal? (list-ref tokenList 1) "==")(equal? (list-ref tokenList 1) "<>")(equal? (list-ref tokenList 1) ">=")(equal? (list-ref tokenList 1) "<=")(equal? (list-ref tokenList 1) ">")(equal? (list-ref tokenList 1) "<")(equal? (list-ref tokenList 1) "if")(equal? (list-ref tokenList 1) "then")(equal? (list-ref tokenList 1) "elseif")(equal? (list-ref tokenList 1) "endif")(equal? (list-ref tokenList 1) "for")(equal? (list-ref tokenList 1) "to")(equal? (list-ref tokenList 1) "stepsize")(equal? (list-ref tokenList 1) "do")(equal? (list-ref tokenList 1) "endfor")(equal? (list-ref tokenList 1) "input")(equal? (list-ref tokenList 1) "output"))
          (display "Please do not use reserved words, numbers, or operators.\n")
          (if (and (not (checkVars (list-ref tokenList 1))) (not (checkFunc (list-ref tokenList 1))))
              (defineVariable (list (list-ref tokenList 1) (list-ref tokenList 2)))
              (display "Variable has already been defined.\n")
              )
          )
      (display "Definition error, please use: '#definevari <varname> <type>'.\n")
      )
  )

;Saves the lines of a function to funcList as a sublist of strings.
(define (defineFunction tokenList)
  ;(display tokenList)(newline)
  (set! funcList (append funcList (list tokenList)))
  ;(display funcList)(newline)
  )

;Takes the token list and checks to make sure it isn't taking any 
(define (constFunc tokenList)
  (set! iValue (read-line))
  (cond [(equal? iValue "#definefunc") (set! tokenList (append tokenList (list iValue))) (defineFunction tokenList) (add_func tokenList)]
;        [(not (number? (index-of (string-split iValue) (list-ref (string-split (list-ref tokenList 0)) 1)))) (begin
;                                                                                                             (display "Recursion is not supported.\n")
;                                                                                                             (constFunc tokenList))]
        [else (begin (set! tokenList (append tokenList (list iValue)))(constFunc tokenList))]
      )
  tokenList
  )

;Takes the start of the function and checks if it already exists or
;is otherwise reserved. Then passes it to another function that will
;allow the user to finish the function.
(define (defFuncHandler tokenString)
  (let ((tokenList (string-split tokenString)))
      (if (or (number? (list-ref tokenList 1))(equal? (list-ref tokenList 1) "+")(equal? (list-ref tokenList 1) "-")(equal? (list-ref tokenList 1) "*")(equal? (list-ref tokenList 1) "/")(equal? (list-ref tokenList 1) "^")(equal? (list-ref tokenList 1) "==")(equal? (list-ref tokenList 1) "<>")(equal? (list-ref tokenList 1) ">=")(equal? (list-ref tokenList 1) "<=")(equal? (list-ref tokenList 1) ">")(equal? (list-ref tokenList 1) "<")(equal? (list-ref tokenList 1) "if")(equal? (list-ref tokenList 1) "then")(equal? (list-ref tokenList 1) "elseif")(equal? (list-ref tokenList 1) "endif")(equal? (list-ref tokenList 1) "for")(equal? (list-ref tokenList 1) "to")(equal? (list-ref tokenList 1) "stepsize")(equal? (list-ref tokenList 1) "do")(equal? (list-ref tokenList 1) "endfor")(equal? (list-ref tokenList 1) "input")(equal? (list-ref tokenList 1) "output"))
      (display "Please do not use reserved words, numbers, or operators.\n")
      (if (or (checkVars (list-ref tokenList 1))(checkFunc (list-ref tokenList 1)))
          (display "Function has already been defined.\n")
          (begin (set! tokenList (list tokenString))(constFunc tokenList)(defineFunction tokenList)) ;May not work as expected.
          )
      )
    )
  )

;Calls the setVariable function using the variable name and the full expression.
(define (varHandler tokenList)
  ;(display tokenList)(newline)
  (let ((backList (cdr (cdr tokenList))))
    (for ([i backList])
      (cond [(checkVars i)(set! backList (list-set backList (index-of backList i) (list-ref (getVariable i) 1)))]
            )
      )
    (if (equal? (list-ref tokenList 1) "=")
        (if (> (length tokenList) 3)
            (begin (varsub backList) (calculate c1) (setVariable (list-ref tokenList 0) (car b1)) (set! c1 (list))) ;(display backList)(newline)
            (begin (setVariable (list-ref tokenList 0) (list-ref backList 0))) ;(display c1)(newline)
            )
        (display "Missing assignment statement. (=)")
        )
    )
  )


;Converst the numbers in a list to strings.
(define (numStr tokenList)
  (for ([i tokenList])
    (if (number? i)
        (set! tokenList (list-set tokenList (index-of tokenList i) (number->string i)))
        '()
        )
    )
  tokenList
  )

;Goes through the contents of a given function, resolving each line according to other functions.
;Can report errors if needed. Also is a great big mess.
(define (funcHandler tokenList)
  ;(display I)(display J)(newline)
  (let ((func '())
        (tokFunc '())
        (argumentValues '())
        (argumentNames '())
        (ifFlag #f)
        (forFlag #f)
        (innerFor #f)
        (forList '())
        (ifList '()))
    (for ([i funcList])
      ;(display (list-ref (string-split (list-ref i 0)) 1))(display " = ")(display (list-ref tokenList 0))(newline)
      (if (equal? (list-ref (string-split (list-ref i 0)) 1) (list-ref tokenList 0))
          (begin (set! func i) (set! argumentNames (cdr (cdr (string-split (car func))))))
          '()
          )
      )
    ;(display (length tokenList))(display " = ")(display (length (cdr (string-split (list-ref func 0)))))(newline)
    (if (equal? (length tokenList) (length (cdr (string-split (list-ref func 0)))))
        (for ([i func])
          ;(display i)(newline)
          (if (and (not (equal? i (car func))) (not (equal? i "#definefunc")))
              (let ((tokens (tokenize i)))
                ;(display tokens)(newline)
                (for ([j tokens])
                  ;(display j)(newline)
                  (for ([k argumentNames])
                    ;(display argumentNames)(newline)
                    (if (equal? j k)
                        (set! tokens (list-set tokens (index-of tokens j) (list-ref argumentValues (index-of argumentNames k))));(display tokens))
                        '()
                        )
                    )
                  )
                (cond 
                        [(and (equal? (list-ref tokens 0) "for")(false? forFlag)) (set! forFlag #t)(set! forList (append forList (list (string-join (numStr tokens)))))]
                        [(and (equal? (list-ref tokens 0) "for")(not (false? forFlag))) (set! innerFor #t)(set! forList (append forList (list (string-join (numStr tokens)))))]
                        [(and (equal? (list-ref tokens 0) "endfor")(not (false? innerFor))) (set! innerFor #f)(set! forList (append forList (list (string-join (numStr tokens)))))]
                        [(and (equal? (list-ref tokens 0) "endfor")(not (false? forFlag))) (set! forFlag #f)(set! forList (append forList (list (string-join (numStr tokens)))))(forProcessing forList)]
                        [(equal? (list-ref tokens 0) "endfor") (display "Unexpected endfor.\n")]
                        [(equal? (list-ref tokens 0) "if") (set! ifFlag #t)(set! ifList (append ifList (list (string-join (numStr tokens)))))]
                        [(and (equal? (list-ref tokens 0) "endif") (not forFlag)) (set! ifFlag #f)(set! ifList (append ifList (list (string-join (numStr tokens)))))(break ifList)]
                        [(and (equal? (list-ref tokens 0) "endif") (not forFlag)) (display "Unexpected endif.\n")]
                        [(not (false? forFlag)) (set! forList (append forList (list (string-join (numStr tokens)))))]
                        [(not (false? ifFlag)) (set! ifList (append ifList (list (string-join (numStr tokens)))))]
                        [(equal? (list-ref tokens 0) "input") (if (checkVars (list-ref tokens 1))
                                                                  (inputHandler (numStr tokens))
                                                                  (display "Can only input into global variables.\n")
                                                                  )]
                        [(equal? (list-ref tokens 0) "output") (if (checkVars (list-ref tokens 1))
                                                                   (outputHandler (numStr tokens))
                                                                   (begin (display (list-ref tokens 1))(newline))
                                                                   )]
                        [(equal? (list-ref tokens 0) "#definevari") (display "Cannot define variables within a function.\n")]
                        [(equal? (list-ref tokens 0) "#definefunc") (display "Cannot define functions within a function.\n")]
                        [(equal? (list-ref tokens 0) "#clear") (display "Cannot clear variables and functions within a function.\n")]
                        [(checkVars (list-ref tokens 0)) (varHandler (tokenize (string-join (numStr tokens))))]
                        [(checkFunc (list-ref tokens 0)) (funcHandler (numStr tokens))] ; (display (numStr tokens))(newline)
                        [else (varsub (tokenize (string-join (numStr tokens)))) (calculate c1)]
                  ))
              (if (equal? i (car func))
                  (for ([j (cdr tokenList)])
                    (set! argumentValues (append argumentValues (list j)))
                    )
                  '()
                  )
            )
          )
        (display "Incorrect number of arguments.\n")
        )
    )
  )

;Loops through the given line of expression and tokenizes it into
;numbers, letters representing variables or possibly functions,
;operators, and parathesis. Returns the resulting list of tokens.
(define (tokenize tokenString)
  (let* ((splitList (list '()))
         (subList (list '()))
         (lastVal ""))
    (set! tokenString (string->list tokenString))
    (set! splitList '())
    (set! subList '())
    (set! lastVal '())
    (for ([i tokenString])
      (cond [(or (char-numeric? i) (char=? i #\.))(begin
                                (cond [(equal? subList '()) (begin (set! subList (list i)) (set! lastVal "digit"))]
                                      [(equal? lastVal "number/expression") (begin (set! subList (append subList (list i))) (set! lastVal "digit"))]
                                      [(equal? lastVal "digit") (set! subList (append subList (list i))) (set! lastVal "digit")]
                                      [(equal? lastVal "") (begin (set! subList (append subList (list i))) (set! lastVal "digit"))]
                                      [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "digit"))]
                                      [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "digit"))]
                                      [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "digit"))]
                                      [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "digit")]
                                      ))]
            [(or (char-alphabetic? i)(char=? i #\#))(begin
                                                      (cond [(equal? subList '()) (begin (set! subList (list i)) (set! lastVal "letter"))]
                                                            [(equal? lastVal "letter") (set! subList (append subList (list i)))]
                                                            [(equal? lastVal "") (begin (set! subList (list i)) (set! lastVal "letter"))]
                                                            [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "letter"))]
                                                            [(equal? lastVal "logic") (begin (set! subList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "letter"))]
                                                            [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "letter"))]
                                                            [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "letter")]
                                                            ))]
            [(char=? i #\()(cond [(equal? subList '()) (set! subList (list i)) (set! lastVal "expression")]
                                        [(equal? lastVal "") (set! subList (list i)) (set! lastVal "expression")]
                                        [(equal? lastVal "number/expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "expression")]
                                        )]
            [(char=? i #\))(cond [(equal? subList '()) (set! subList (list i)) (set! lastVal "expression")]
                                        [(equal? lastVal "") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "number/expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "expression")]
                                        )]
            [(char=? i #\+)(cond [(equal? subList '()) (set! subList (list i)) (set! lastVal "expression")]
                                        [(equal? lastVal "") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "number/expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "expression")]
                                        )]
            [(char=? i #\-)(begin (cond [(equal? subList '()) (begin (set! subList (list i)) (set! lastVal "number/expression"))]
                                        [(equal? lastVal "") (begin (set! subList (append subList (list i))) (set! lastVal "number/expression"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "number/expression"))]
                                        [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "number/expression"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "number/expression"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "number/expression"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "number/expression")]
                                        ))]
            [(char=? i #\*)(begin (cond [(equal? subList '()) (set! subList (list i)) (set! lastVal "expression")]
                                        [(equal? lastVal "") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "number/expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "expression")]
                                        ))]
            [(char=? i #\/)(begin (cond [(equal? subList '()) (set! subList (list i)) (set! lastVal "expression")]
                                        [(equal? lastVal "") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "number/expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "expression")]
                                        ))]
            [(char=? i #\^)(begin (cond [(equal? subList '()) (set! subList (list i)) (set! lastVal "expression")]
                                        [(equal? lastVal "") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "number/expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "expression"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "expression")]
                                        ))]
            [(char=? i #\=)(begin (cond [(equal? subList '()) (begin (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "logic") (set! subList (append subList (list i)))]
                                        [(equal? lastVal "") (begin (set! subList (append subList (list i))) (set! lastVal "logic"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "digit"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "logic")]
                                        ))]
            [(char=? i #\<)(begin (cond [(equal? subList '()) (begin (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "logic") (set! subList (append subList (list i)))]
                                        [(equal? lastVal "") (begin (set! subList (append subList (list i))) (set! lastVal "logic"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "logic")]
                                        ))]
            [(char=? i #\>)(begin (cond [(equal? subList '()) (begin (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "logic") (set! subList (append subList (list i)))]
                                        [(equal? lastVal "") (begin (set! subList (append subList (list i))) (set! lastVal "logic"))]
                                        [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList (list i)) (set! lastVal "logic"))]
                                        [(equal? lastVal "space") (set! subList (list i)) (set! lastVal "logic")]
                                        ))]
            [(char=? i #\space)(begin (cond [(equal? subList '()) (set! lastVal "space")]
                                            [(equal? lastVal "number/expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList '()) (set! lastVal "space"))]
                                            [(equal? lastVal "logic") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList '()) (set! lastVal "space"))]
                                            [(equal? lastVal "") (set! subList '()) (set! lastVal "space")]
                                            [(equal? lastVal "digit") (begin (set! splitList (append splitList (list (string->number (list->string subList))))) (set! subList '()) (set! lastVal "space"))]
                                            [(equal? lastVal "letter") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList '()) (set! lastVal "space"))]
                                            [(equal? lastVal "expression") (begin (set! splitList (append splitList (list (list->string subList)))) (set! subList '()) (set! lastVal "space"))]
                                            [(equal? lastVal "space") (set! subList '()) (set! lastVal "space")]
                                            ))]
            [else (display "Error, unresolved character.\n")]
            )
      ;(display subList)(display "; ")(display splitList)(display "; ")(display lastVal)(newline)
      )
    (cond [(equal? lastVal "digit") (set! splitList (append splitList (list (string->number (list->string subList)))))]
          [(equal? lastVal "letter") (set! splitList (append splitList (list (list->string subList))))]
          [(equal? lastVal "logic") (set! splitList (append splitList (list (list->string subList))))]
          [(not (empty? subList)) (set! splitList (append splitList (list (list->string subList))))]
          )
    ;(display splitList)(newline)
    splitList
    )
  )

;Function tokenizes the input line from the main function.
;It will then check the first string in the list of tokens to see
;which function it should pass control over to next.
(define (chooseTask inValue)
  ;(display (string-append inValue "\n"))
  (set! tList (string-split inValue))
  ;(display tList)(newline)
  (set! b1 (list))
  (cond [(equal? tList '()) (display "Please input a value.\n")]
        [(equal? (list-ref tList 0) "if") (ifHandler (list (string-join tList)))]
        [(equal? (list-ref tList 0) "for") (forHandler (list (string-join tList)))]
        [(equal? (list-ref tList 0) "input") (inputHandler tList)]
        [(equal? (list-ref tList 0) "output") (outputHandler tList)]
        [(equal? (list-ref tList 0) "#definevari") (defVarHandler tList)]
        [(equal? (list-ref tList 0) "#definefunc") (defFuncHandler (string-join tList))]
        [(equal? (list-ref tList 0) "#clear") (clear)]
        [(and (> (length tList) 2)(equal? (list-ref tList 1) "=")(checkVars (list-ref tList 0))) (varHandler (tokenize (string-join tList)))]
        [(checkFunc (list-ref tList 0)) (funcHandler tList)]
        [else (calculate (tokenize (string-join tList))) (display b1)(newline)]
        )
  )

;Recursivly takes in input until the input is #exit.
;Will pass the input to other functions for processing.
(define (displayLoop)
  (display "UofL>")
  (set! iValue (read-line))
  (if (not (equal? iValue "#exit"))
      (begin (chooseTask iValue)(displayLoop))
      (begin (display "Calculator shutting down.\n")(saveVars)(set! varList '())(set! funcList '()))
      )
  )

;Main function, loads the list of functions and variables, then
;passes control to a looping display.
(define (uofl)
  (read_var)
  ;(display varList)
  (newline)
  (read_func)
  ;(display funcList)
  (newline)
  (displayLoop)
)