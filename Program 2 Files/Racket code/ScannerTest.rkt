#lang racket

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

(define (read-word first-char)
  (let loop ((chars (list first-char)) (next-char (read-char)))
    (if (and (not (eof-object? next-char)) 
             (or (char-alphabetic? next-char) (char-numeric? next-char)))
        (loop (cons next-char chars) (read-char))
        (values (list->string (reverse chars)) next-char))))

(define (read-number first-char)
  (let loop ((chars (list first-char)) (next-char (read-char)))
    (if (and (not (eof-object? next-char)) (char-numeric? next-char))
        (loop (cons next-char chars) (read-char))
        (values (list->string (reverse chars)) next-char))))

(define (read-string first-char)
  (let loop ((chars '()) (next-char (read-char)))
    (cond
      [(eof-object? next-char) (error "Unterminated string")]
      [(char=? next-char #\") (values (list->string (reverse chars)) (read-char))]
      [else (loop (cons next-char chars) (read-char))])))

(define (categorize-word word)
  (let ([upper-word (string-upcase word)])
    (cond
      [(member upper-word '("IF" "THEN" "ELSE" "WHILE" "DO" "END" "PRINT" "INPUT" "RETURN" "ENDDEF" "ENDIF" "DEF" "OR" "AND" "NOT"))
       `(keyword ,upper-word)]
      [else `(id ,word)])))

(define (skip-line)
  (let loop ((char (read-char)))
    (if (or (eof-object? char) (char=? char #\newline))
        (read-char)
        (loop (read-char)))))

(define (handle-unknown-char char)
  (error (format "Unknown character: ~a" char)))

(define (scan-file filename)
  (with-handlers ([exn:fail? (lambda (exn) (display (exn-message exn)) (newline))])
    (let ([tokens (tokenize-file filename)])
      (pretty-print tokens)
      (newline))))

(scan-file "Fall24SampleCode.txt")



