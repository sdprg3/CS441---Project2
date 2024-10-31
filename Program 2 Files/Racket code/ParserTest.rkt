#lang racket

;; Parser functions

(define (parse-program tokens)
  (let ([statements (let loop ([remaining-tokens tokens]
                               [statements '()])
                      (if (null? remaining-tokens)
                          (reverse statements)
                          (let-values ([(stmt rest) (parse-statement remaining-tokens)])
                            (loop rest (cons stmt statements)))))])
    `(STMT ,@statements)))

(define (parse-statement tokens)
  (match tokens
    [(list (list 'keyword "DEF") rest ...)
     (parse-def-statement tokens)]
    [(list (list 'keyword "IF") rest ...)
     (parse-if-statement tokens)]
    [(list (list 'keyword "THEN") rest ...)
     (values '(then) rest)]  ; Add this line to handle THEN keyword
    [(list (list 'id _) rest ...)
     (parse-id-statement tokens)]
    [(list (list 'keyword "PRINT") rest ...)
     (parse-print-statement tokens)]
    [(list (list 'keyword "WHILE") rest ...)
     (parse-while-statement tokens)]
    [(list (list 'keyword "DO") rest ...)
     (values '(do) rest)]
    [(list (list 'keyword "RETURN") rest ...)
     (parse-return-statement tokens)]
    [(list (list 'keyword "END") rest ...)
     (values '(end) rest)]
    [(list (list 'keyword "ENDWHILE") rest ...)
     (values '(endwhile) rest)]
    [(list (list 'keyword "ENDIF") rest ...)
     (values '(endif) rest)]
    [(list (list 'keyword "ENDDEF") rest ...)
     (values '(enddef) rest)]
    [(list (list 'delimiter "colon") rest ...)
     (values '(statement-separator) rest)]
    [(list (list 'delimiter _) rest ...)
     (parse-delimiter tokens)]
    [(list (list 'operator _) rest ...)
     (parse-expression tokens)]
    [(list (list 'number _) rest ...)
     (parse-expression tokens)]
    [(list (list 'string _) rest ...)
     (parse-expression tokens)]
    [else (error (format "Unexpected token sequence: ~a" (car tokens)))]))

(define (parse-id-statement tokens)
  (parse-expression tokens))

(define (parse-operator-statement tokens)
  (match tokens
    [(list (list 'operator op) rest ...)
     (values `(operator ,op) rest)]
    [else (error (format "Invalid operator statement: ~a" (car tokens)))]))

(define (parse-delimiter tokens)
  (match tokens
    [(list (list 'delimiter delim) rest ...)
     (values `(delimiter ,delim) rest)]
    [else (error (format "Invalid delimiter: ~a" (car tokens)))]))

(define (parse-delimiter-statement tokens)
  (match tokens
    [(list (list 'delimiter "colon") rest ...)
     (values '(statement-separator) rest)]
    [(list (list 'delimiter delim) rest ...)
     (parse-expression tokens)]
    [else (error (format "Invalid delimiter statement: ~a" (car tokens)))]))

(define (parse-string-statement tokens)
  (match tokens
    [(list (list 'string str) rest ...)
     (values `(string ,str) rest)]
    [else (error (format "Invalid string statement: ~a" (car tokens)))]))

(define (parse-def-statement tokens)
  (let*-values ([(name params) (parse-def-header tokens)]
                [(body rest1) (parse-statements (cdr (cdr (cdr (cdr tokens)))))]
                [(enddef rest2) (match rest1
                                  [(list (list 'keyword "ENDDEF") more ...) (values 'enddef more)]
                                  [else (error "Expected ENDDEF")])])
    (values `(def ,name ,params ,body) rest2)))

(define (parse-def-header tokens)
  (match tokens
    [(list (list 'keyword "DEF") (list 'id name) (list 'delimiter "lparen") rest ...)
     (let-values ([(params rest) (parse-id-list rest)])
       (values name params))]
    [(list (list 'keyword "DEF") (list 'id name) rest ...)
     (values name '())]
    [else (error "Invalid DEF statement")]))

(define (parse-id-list tokens)
  (let loop ([remaining tokens]
             [params '()])
    (match remaining
      [(list (list 'id name) (list 'delimiter "comma") rest ...)
       (loop rest (cons name params))]
      [(list (list 'id name) (list 'delimiter "rparen") rest ...)
       (values (reverse (cons name params)) rest)]
      [(list (list 'delimiter "rparen") rest ...)
       (values (reverse params) rest)]
      [else (error "Invalid parameter list")])))

(define (parse-if-statement tokens)
  (let*-values ([(condition rest1) (parse-expression (cdr tokens))]
                [(then-part rest2) (parse-statements rest1)]
                [(else-part rest3) (parse-else-part rest2)]
                [(endif rest4) (match rest3
                                 [(list (list 'keyword "ENDIF") more ...) (values 'endif more)]
                                 [else (error "Expected ENDIF")])])
    (values `(if ,condition ,then-part ,else-part) rest4)))

(define (parse-else-part tokens)
  (match tokens
    [(list (list 'keyword "ELSE") rest ...)
     (let-values ([(else-part remaining) (parse-statements rest)])
       (values else-part remaining))]
    [else (values '() tokens)]))

(define (parse-assignment-statement tokens)
  (match tokens
    [(list (list 'id name) (list 'operator "assign") rest ...)
     (let-values ([(expr remaining) (parse-expression rest)])
       (values `(assign ,name ,expr) remaining))]
    [else (error "Invalid assignment statement")]))

(define (parse-assignment-expression tokens)
  (let-values ([(left rest) (parse-or-expression tokens)])
    (match rest
      [(list (list 'operator "assign") more ...)
       (let-values ([(right remaining) (parse-assignment-expression more)])
         (values `(assign ,left ,right) remaining))]
      [_ (values left rest)])))

(define (parse-function-call tokens)
  (match tokens
    [(list (list 'id name) (list 'delimiter "lparen") rest ...)
     (let-values ([(args remaining) (parse-argument-list rest)])
       (values `(function-call ,name ,args) remaining))]
    [else (error "Invalid function call")]))

(define (parse-print-statement tokens)
  (let-values ([(expr-list rest) (parse-print-list (cdr tokens))])
    (values `(print ,expr-list) rest)))

(define (parse-print-list tokens)
  (let loop ([remaining tokens]
             [exprs '()])
    (let-values ([(expr rest) (parse-expression remaining)])
      (match rest
        [(list (list 'delimiter "semicolon") more ...)
         (loop more (cons expr exprs))]
        [_ (values (reverse (cons expr exprs)) rest)]))))

(define (parse-while-statement tokens)
  (let*-values ([(condition rest1) (parse-expression (cdr tokens))]
                [(do-keyword rest2) (match rest1
                                      [(list (list 'keyword "DO") more ...) (values 'do more)]
                                      [else (values 'do rest1)])]  ; Assume 'DO' if not present
                [(body rest3) (parse-statements rest2)]
                [(endwhile rest4) (match rest3
                                    [(list (list 'keyword "ENDWHILE") more ...) (values 'endwhile more)]
                                    [else (error "Expected ENDWHILE")])])
    (values `(while ,condition ,body) rest4)))

(define (parse-return-statement tokens)
  (let-values ([(expr rest) (parse-expression (cdr tokens))])
    (values `(return ,expr) rest)))

(define (parse-expression tokens)
  (let-values ([(left rest) (parse-value tokens)])
    (match rest
      [(list (list 'operator op) more ...)
       (let-values ([(right remaining) (parse-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

(define (parse-expression-tail left tokens)
  (match tokens
    [(list (list 'operator op) more ...)
     (let-values ([(right remaining) (parse-expression more)])
       (values `(,op ,left ,right) remaining))]
    [_ (values left tokens)]))

(define (parse-or-expression tokens)
  (let-values ([(left rest) (parse-and-expression tokens)])
    (match rest
      [(list (list 'keyword "OR") more ...)
       (let-values ([(right remaining) (parse-or-expression more)])
         (values `(or ,left ,right) remaining))]
      [_ (values left rest)])))

(define (parse-and-expression tokens)
  (let-values ([(left rest) (parse-not-expression tokens)])
    (match rest
      [(list (list 'keyword "AND") more ...)
       (let-values ([(right remaining) (parse-and-expression more)])
         (values `(and ,left ,right) remaining))]
      [_ (values left rest)])))
(define (parse-not-expression tokens)
  (match tokens
    [(list (list 'keyword "NOT") rest ...)
     (let-values ([(expr remaining) (parse-not-expression rest)])
       (values `(not ,expr) remaining))]
    [_ (parse-compare-expression tokens)]))

(define (parse-compare-expression tokens)
  (let-values ([(left rest) (parse-add-expression tokens)])
    (match rest
      [(list (list 'operator op) more ...) #:when (member op '("equals" "notequal" "greaterthan" "greaterequal" "lessthan" "lessequal"))
       (let-values ([(right remaining) (parse-add-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

(define (parse-add-expression tokens)
  (let-values ([(left rest) (parse-mult-expression tokens)])
    (match rest
      [(list (list 'operator op) more ...) #:when (member op '("plus" "minus"))
       (let-values ([(right remaining) (parse-add-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

(define (parse-mult-expression tokens)
  (let-values ([(left rest) (parse-negate-expression tokens)])
    (match rest
      [(list (list 'operator op) more ...) #:when (member op '("times" "divide"))
       (let-values ([(right remaining) (parse-mult-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

(define (parse-negate-expression tokens)
  (match tokens
    [(list (list 'operator "minus") rest ...)
     (let-values ([(expr remaining) (parse-value tokens)])
       (values `(negate ,expr) remaining))]
    [_ (parse-value tokens)]))

(define (parse-value tokens)
  (match tokens
    [(list (list 'delimiter "lparen") rest ...)
     (let-values ([(expr remaining) (parse-expression rest)])
       (match remaining
         [(list (list 'delimiter "rparen") more ...)
          (values expr more)]
         [_ (error "Mismatched parentheses")]))]
    [(list (list 'id name) (list 'delimiter "lparen") rest ...)
     (parse-function-call tokens)]
    [(list (list 'id name) rest ...)
     (values name rest)]
    [(list (list 'number val) rest ...)
     (values val rest)]
    [(list (list 'string val) rest ...)
     (values val rest)]
    [(list (list 'operator op) rest ...)
     (values op rest)]
    [(list (list 'delimiter delim) rest ...)
     (values delim rest)]
    [else (error (format "Invalid value: ~a" (car tokens)))]))

(define (parse-argument-list tokens)
  (let loop ([remaining tokens]
             [args '()])
    (let-values ([(arg rest) (parse-expression remaining)])
      (match rest
        [(list (list 'delimiter "comma") more ...)
         (loop more (cons arg args))]
        [(list (list 'delimiter "rparen") more ...)
         (values (reverse (cons arg args)) more)]
        [_ (error "Invalid argument list")]))))

(define (parse-statements tokens)
  (let loop ([remaining tokens]
             [stmts '()])
    (if (or (null? remaining)
            (equal? (car remaining) '(keyword "ENDIF"))
            (equal? (car remaining) '(keyword "ENDWHILE"))
            (equal? (car remaining) '(keyword "ENDDEF"))
            (equal? (car remaining) '(keyword "END")))
        (values (reverse stmts) remaining)
        (let-values ([(stmt rest) (parse-statement remaining)])
          (loop rest (cons stmt stmts))))))

;; Main parsing function
(define (parse tokens)
  (parse-program tokens))

;;    ))'((

;; Test the parser
(define sample-tokens
  '((id "x")
    (operator assign)
    (number "5")
    (id "y")
    (operator assign)
    (id "x")
    (operator plus)
    (number "1")
    (delimiter colon)
    (id "z")
    (operator assign)
    (delimiter lparen)
    (id "y")
    (operator minus)
    (number "2")
    (delimiter rparen)
    (operator times)
    (number "3")
    (id "A")
    (operator assign)
    (id "somefunction")
    (delimiter lparen)
    (id "z")
    (delimiter rparen)
    (keyword "PRINT")
    (delimiter lparen)
    (id "z")
    (operator times)
    (number "2")
    (delimiter rparen)
    (keyword "PRINT")
    (string "A = ")
    (delimiter semicolon)
    (id "A")
    (keyword "END")
    (keyword "DEF")
    (id "somefunction")
    (delimiter lparen)
    (id "a")
    (delimiter rparen)
    (keyword "WHILE")
    (id "a")
    (operator lessequal)
    (number "0")
    (keyword "DO")
    (id "a")
    (operator assign)
    (id "a")
    (operator plus)
    (number "10")
    (keyword "ENDWHILE")
    (delimiter colon)
    (keyword "IF")
    (id "a")
    (operator greaterthan)
    (number "5")
    (keyword "THEN")
    (id "a")
    (operator assign)
    (id "a")
    (operator plus)
    (number "2")
    (keyword "RETURN")
    (id "a")
    (operator times)
    (number "3")
    (keyword "ENDIF")
    (keyword "RETURN")
    (id "a")
    (delimiter colon)
    (keyword "ENDDEF")))

(pretty-print (parse sample-tokens))