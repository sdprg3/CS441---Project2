#lang racket

;;Scanner

;; Main function to tokenize a file
;; Takes a filename as input and returns a list of tokens
(define (tokenize-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((char (read-char)) (tokens '()))
        (cond
          [(eof-object? char) (reverse tokens)]
          [(char-whitespace? char) (loop (read-char) tokens)]
          [(char-alphabetic? char) 
           (let-values ([(word rest) (read-word char)])
             (if (string-ci=? word "REM")
                 (loop (skip-line) tokens)
                 (loop rest (cons (categorize-word word) tokens))))]
          [(char-numeric? char)
           (let-values ([(number rest) (read-number char)])
             (loop rest (cons `(number ,number) tokens)))]
          [else
           (case char
             ;; Handle various operators and delimiters
             [(#\:) (if (char=? (peek-char) #\=)
                        (begin (read-char) (loop (read-char) (cons '(operator assign) tokens)))
                        (loop (read-char) (cons `(delimiter colon) tokens)))]
             [(#\+) (loop (read-char) (cons '(operator plus) tokens))]
             [(#\-) (loop (read-char) (cons '(operator minus) tokens))]
             [(#\*) (loop (read-char) (cons '(operator times) tokens))]
             [(#\/) (loop (read-char) (cons '(operator divide) tokens))]
             [(#\=) (loop (read-char) (cons '(operator equals) tokens))]
             [(#\<) (cond
                      [(char=? (peek-char) #\=) (read-char) (loop (read-char) (cons '(operator lessequal) tokens))]
                      [(char=? (peek-char) #\>) (read-char) (loop (read-char) (cons '(operator notequal) tokens))]
                      [else (loop (read-char) (cons '(operator lessthan) tokens))])]
             [(#\>) (cond
                      [(char=? (peek-char) #\=) (read-char) (loop (read-char) (cons '(operator greaterequal) tokens))]
                      [(char=? (peek-char) #\<) (read-char) (loop (read-char) (cons '(operator notequal) tokens))]
                      [else (loop (read-char) (cons '(operator greaterthan) tokens))])]
             [(#\() (loop (read-char) (cons `(delimiter lparen) tokens))]
             [(#\)) (loop (read-char) (cons `(delimiter rparen) tokens))]
             [(#\;) (loop (read-char) (cons `(delimiter semicolon) tokens))]
             [(#\,) (loop (read-char) (cons `(delimiter comma) tokens))]
             [(#\") (let-values ([(string rest) (read-string char)])
                      (loop rest (cons `(string ,string) tokens)))]
             [else (handle-unknown-char char)])])))))

;; Function to read a word (identifier or keyword)
;; Takes the first character of the word and returns the full word and the next character
(define (read-word first-char)
  (let loop ((chars (list first-char)) (next-char (read-char)))
    (if (and (not (eof-object? next-char)) 
             (or (char-alphabetic? next-char) (char-numeric? next-char)))
        (loop (cons next-char chars) (read-char))
        (values (list->string (reverse chars)) next-char))))

;; Function to read a number
;; Takes the first digit of the number and returns the full number as a string and the next character
(define (read-number first-char)
  (let loop ((chars (list first-char)) (next-char (read-char)))
    (if (and (not (eof-object? next-char)) (char-numeric? next-char))
        (loop (cons next-char chars) (read-char))
        (values (list->string (reverse chars)) next-char))))

;; Function to read a string
;; Takes the opening quote and returns the string content and the character after the closing quote
(define (read-string first-char)
  (let loop ((chars '()) (next-char (read-char)))
    (cond
      [(eof-object? next-char) (error "Unterminated string")]
      [(char=? next-char #\") (values (list->string (reverse chars)) (read-char))]
      [else (loop (cons next-char chars) (read-char))])))

;; Function to categorize a word as a keyword or identifier
;; Takes a word and returns a token of the form (keyword WORD) or (id word)
(define (categorize-word word)
  (let ([upper-word (string-upcase word)])
    (cond
      [(member upper-word '("IF" "THEN" "ELSE" "WHILE" "DO" "END" "PRINT" "INPUT" "RETURN" "ENDDEF" "ENDIF" "DEF" "OR" "AND" "NOT" "ENDWHILE"))
       `(keyword ,upper-word)]
      [else `(id ,word)])))

;; Function to skip a line (used for comments)
;; Reads characters until a newline or EOF is encountered
(define (skip-line)
  (let loop ((char (read-char)))
    (if (or (eof-object? char) (char=? char #\newline))
        (read-char)
        (loop (read-char)))))

;; Function to handle unknown characters
;; Raises an error with the unknown character
(define (handle-unknown-char char)
  (error (format "Unknown character: ~a" char)))

;; Function to scan a file and print the tokens
;; Takes a filename, tokenizes it, and pretty-prints the resulting tokens
(define (scan-file filename)
  (with-handlers ([exn:fail? (lambda (exn) (display (exn-message exn)) (newline))])
    (let ([tokens (tokenize-file filename)])
      (pretty-print tokens)
      (newline))))

;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------

;; Parser functions

;; Parses the entire program, creating a list of statements
(define (parse-program tokens)
  (let ([statements (let loop ([remaining-tokens tokens]
                               [statements '()])
                      (if (null? remaining-tokens)
                          (reverse statements)
                          (let-values ([(stmt rest) (parse-statement remaining-tokens)])
                            (loop rest (cons stmt statements)))))])
    `(PROGRAM ,@statements)))


;; Parses a single statement, dispatching to specific statement parsers based on the token type
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

;; Parses an identifier statement (which is treated as an expression)
(define (parse-id-statement tokens)
  (parse-expression tokens))

;; Parses an operator statement
(define (parse-operator-statement tokens)
  (match tokens
    [(list (list 'operator op) rest ...)
     (values `(operator ,op) rest)]
    [else (error (format "Invalid operator statement: ~a" (car tokens)))]))

;; Parses a delimiter token
(define (parse-delimiter tokens)
  (match tokens
    [(list (list 'delimiter delim) rest ...)
     (values `(delimiter ,delim) rest)]
    [else (error (format "Invalid delimiter: ~a" (car tokens)))]))

;; Parses a delimiter statement
(define (parse-delimiter-statement tokens)
  (match tokens
    [(list (list 'delimiter "colon") rest ...)
     (values '(statement-separator) rest)]
    [(list (list 'delimiter delim) rest ...)
     (parse-expression tokens)]
    [else (error (format "Invalid delimiter statement: ~a" (car tokens)))]))

;; Parses a string statement
(define (parse-string-statement tokens)
  (match tokens
    [(list (list 'string str) rest ...)
     (values `(string ,str) rest)]
    [else (error (format "Invalid string statement: ~a" (car tokens)))]))

; Parses a function definition statement
(define (parse-def-statement tokens)
  (let*-values ([(name params) (parse-def-header tokens)]
                [(body rest1) (parse-statements (cdr (cdr (cdr (cdr tokens)))))]
                [(enddef rest2) (match rest1
                                  [(list (list 'keyword "ENDDEF") more ...) (values 'enddef more)]
                                  [else (error "Expected ENDDEF")])])
    (values `(def ,name ,params ,body) rest2)))

;; Parses the header of a function definition (name and parameters)
(define (parse-def-header tokens)
  (match tokens
    [(list (list 'keyword "DEF") (list 'id name) (list 'delimiter "lparen") rest ...)
     (let-values ([(params rest) (parse-id-list rest)])
       (values name params))]
    [(list (list 'keyword "DEF") (list 'id name) rest ...)
     (values name '())]
    [else (error "Invalid DEF statement")]))

;; Parses a list of identifiers (used for function parameters)
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

;; Parses an if statement
(define (parse-if-statement tokens)
  (let*-values ([(condition rest1) (parse-expression (cdr tokens))]
                [(then-part rest2) (parse-statements rest1)]
                [(else-part rest3) (parse-else-part rest2)]
                [(endif rest4) (match rest3
                                 [(list (list 'keyword "ENDIF") more ...) (values 'endif more)]
                                 [else (error "Expected ENDIF")])])
    (values `(if ,condition ,then-part ,else-part) rest4)))

;; Parses the else part of an if statement
(define (parse-else-part tokens)
  (match tokens
    [(list (list 'keyword "ELSE") rest ...)
     (let-values ([(else-part remaining) (parse-statements rest)])
       (values else-part remaining))]
    [else (values '() tokens)]))

;; Parses an assignment statement
(define (parse-assignment-statement tokens)
  (match tokens
    [(list (list 'id name) (list 'operator "assign") rest ...)
     (let-values ([(expr remaining) (parse-expression rest)])
       (values `(assign ,name ,expr) remaining))]
    [else (error "Invalid assignment statement")]))

;; Parses an assignment expression
(define (parse-assignment-expression tokens)
  (let-values ([(left rest) (parse-or-expression tokens)])
    (match rest
      [(list (list 'operator "assign") more ...)
       (let-values ([(right remaining) (parse-assignment-expression more)])
         (values `(assign ,left ,right) remaining))]
      [_ (values left rest)])))

;; Parses a function call
(define (parse-function-call tokens)
  (match tokens
    [(list (list 'id name) (list 'delimiter "lparen") rest ...)
     (let-values ([(args remaining) (parse-argument-list rest)])
       (values `(function-call ,name ,args) remaining))]
    [else (error "Invalid function call")]))

;; Parses a print statement
(define (parse-print-statement tokens)
  (let-values ([(expr-list rest) (parse-print-list (cdr tokens))])
    (values `(print ,expr-list) rest)))

;; Parses the list of expressions in a print statement
(define (parse-print-list tokens)
  (let loop ([remaining tokens]
             [exprs '()])
    (let-values ([(expr rest) (parse-expression remaining)])
      (match rest
        [(list (list 'delimiter "semicolon") more ...)
         (loop more (cons expr exprs))]
        [_ (values (reverse (cons expr exprs)) rest)]))))

;; Parses a while statement
(define (parse-while-statement tokens)
  (let*-values ([(condition rest1) (parse-expression (cdr tokens))]
                [(do-keyword rest2) (match rest1
                                      [(list (list 'keyword "DO") more ...) (values 'do more)]
                                      [else (error "Expected DO after WHILE condition")])]
                [(body rest3) (parse-statements rest2)]
                [(endwhile rest4) (match rest3
                                    [(list (list 'keyword "ENDWHILE") more ...) (values 'endwhile more)]
                                    [else (error "Expected ENDWHILE at the end of WHILE loop")])])
    (values `(while ,condition ,body) rest4)))


;; Parses a return statement
(define (parse-return-statement tokens)
  (let-values ([(expr rest) (parse-expression (cdr tokens))])
    (values `(return ,expr) rest)))

;; Parses an expression
(define (parse-expression tokens)
  (let-values ([(left rest) (parse-value tokens)])
    (match rest
      [(list (list 'operator op) more ...)
       (let-values ([(right remaining) (parse-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

;; Parses an expression
(define (parse-expression-tail left tokens)
  (match tokens
    [(list (list 'operator op) more ...)
     (let-values ([(right remaining) (parse-expression more)])
       (values `(,op ,left ,right) remaining))]
    [_ (values left tokens)]))


;; Parses an OR expression
(define (parse-or-expression tokens)
  (let-values ([(left rest) (parse-and-expression tokens)])
    (match rest
      [(list (list 'keyword "OR") more ...)
       (let-values ([(right remaining) (parse-or-expression more)])
         (values `(or ,left ,right) remaining))]
      [_ (values left rest)])))

;; Parses an AND expression
(define (parse-and-expression tokens)
  (let-values ([(left rest) (parse-not-expression tokens)])
    (match rest
      [(list (list 'keyword "AND") more ...)
       (let-values ([(right remaining) (parse-and-expression more)])
         (values `(and ,left ,right) remaining))]
      [_ (values left rest)])))

;; Parses a NOT expression
(define (parse-not-expression tokens)
  (match tokens
    [(list (list 'keyword "NOT") rest ...)
     (let-values ([(expr remaining) (parse-not-expression rest)])
       (values `(not ,expr) remaining))]
    [_ (parse-compare-expression tokens)]))

;; Parses a comparison expression
(define (parse-compare-expression tokens)
  (let-values ([(left rest) (parse-add-expression tokens)])
    (match rest
      [(list (list 'operator op) more ...) #:when (member op '("equals" "notequal" "greaterthan" "greaterequal" "lessthan" "lessequal"))
       (let-values ([(right remaining) (parse-add-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

;; Parses an addition or subtraction expression
(define (parse-add-expression tokens)
  (let-values ([(left rest) (parse-mult-expression tokens)])
    (match rest
      [(list (list 'operator op) more ...) #:when (member op '("plus" "minus"))
       (let-values ([(right remaining) (parse-add-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

;; Parses a multiplication or division expression
(define (parse-mult-expression tokens)
  (let-values ([(left rest) (parse-negate-expression tokens)])
    (match rest
      [(list (list 'operator op) more ...) #:when (member op '("times" "divide"))
       (let-values ([(right remaining) (parse-mult-expression more)])
         (values `(,op ,left ,right) remaining))]
      [_ (values left rest)])))

;; Parses a negation expression
(define (parse-negate-expression tokens)
  (match tokens
    [(list (list 'operator "minus") rest ...)
     (let-values ([(expr remaining) (parse-value tokens)])
       (values `(negate ,expr) remaining))]
    [_ (parse-value tokens)]))

;; Parses a value (number, string, identifier, or parenthesized expression)
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

;; Parses a list of arguments for a function call
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

;; Parses a sequence of statements
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

;; Function to scan and parse a file
(define (scan-and-parse-file filename)
  (with-handlers ([exn:fail? (lambda (exn) 
                               (display (exn-message exn)) 
                               (newline))])
    (let ([tokens (tokenize-file filename)])
      (display "Tokens:\n")
      (pretty-print tokens)
      (newline)
      (display "Parsed result:\n")
      (pretty-print (parse tokens))
      (newline))))

;; Test the scanner and parser
(scan-and-parse-file "Fall24SampleCode.txt")

