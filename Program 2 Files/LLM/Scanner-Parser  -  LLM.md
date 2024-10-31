# #lang racket...
Exported on 30/10/2024 at 23:12:48 [from Perplexity.ai](https://www.perplexity.ai/search/program2-part-3-2120AXLqT9mKYa8WiHKENQ) - with [SaveMyChatbot](https://save.hugocollin.com)


## User
#lang racket  
  
(define (tokenize-file filename)  
  (with-input-from-file filename  
    (lambda ()  
      (let loop ((char (read-char)) (tokens '()))  
        (cond  
          \[(eof-object? char) (reverse tokens)\]  
          \[(char-whitespace? char) (loop (read-char) tokens)\]  
          \[(char-alphabetic? char)  
           (let-values (\[(word rest) (read-word char)\])  
             (if (string-ci=? word "REM")  
                 (loop (skip-line) tokens)  
                 (loop rest (cons (categorize-word word) tokens))))\]  
          \[(char-numeric? char)  
           (let-values (\[(number rest) (read-number char)\])  
             (loop rest (cons \`(number ,number) tokens)))\]  
          \[else  
           (case char  
             \[(#\\:) (if (char=? (peek-char) #\\=)  
                        (begin (read-char) (loop (read-char) (cons '(operator assign) tokens)))  
                        (loop (read-char) (cons \`(delimiter colon) tokens)))\]  
             \[(#\\+) (loop (read-char) (cons '(operator plus) tokens))\]  
             \[(#\\-) (loop (read-char) (cons '(operator minus) tokens))\]  
             \[(#\\\*) (loop (read-char) (cons '(operator times) tokens))\]  
             \[(#\\/) (loop (read-char) (cons '(operator divide) tokens))\]  
             \[(#\\=) (loop (read-char) (cons '(operator equals) tokens))\]  
             \[(#\\<) (cond  
                      \[(char=? (peek-char) #\\=) (read-char) (loop (read-char) (cons '(operator lessequal) tokens))\]  
                      \[(char=? (peek-char) #\\>) (read-char) (loop (read-char) (cons '(operator notequal) tokens))\]  
                      \[else (loop (read-char) (cons '(operator lessthan) tokens))\])\]  
             \[(#\\>) (cond  
                      \[(char=? (peek-char) #\\=) (read-char) (loop (read-char) (cons '(operator greaterequal) tokens))\]  
                      \[(char=? (peek-char) #\\<) (read-char) (loop (read-char) (cons '(operator notequal) tokens))\]  
                      \[else (loop (read-char) (cons '(operator greaterthan) tokens))\])\]  
             \[(#\\() (loop (read-char) (cons \`(delimiter lparen) tokens))\]  
             \[(#\\)) (loop (read-char) (cons \`(delimiter rparen) tokens))\]  
             \[(#\\;) (loop (read-char) (cons \`(delimiter semicolon) tokens))\]  
             \[(#\\,) (loop (read-char) (cons \`(delimiter comma) tokens))\]  
             \[(#\\") (let-values (\[(string rest) (read-string char)\])  
                      (loop rest (cons \`(string ,string) tokens)))\]  
             \[else (handle-unknown-char char)\])\])))))  
  
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
      \[(eof-object? next-char) (error "Unterminated string")\]  
      \[(char=? next-char #\\") (values (list->string (reverse chars)) (read-char))\]  
      \[else (loop (cons next-char chars) (read-char))\])))  
  
(define (categorize-word word)  
  (let (\[upper-word (string-upcase word)\])  
    (cond  
      \[(member upper-word '("IF" "THEN" "ELSE" "WHILE" "DO" "END" "PRINT" "INPUT" "RETURN" "ENDDEF" "ENDIF" "DEF" "OR" "AND" "NOT"))  
       \`(keyword ,upper-word)\]  
      \[else \`(id ,word)\])))  
  
(define (skip-line)  
  (let loop ((char (read-char)))  
    (if (or (eof-object? char) (char=? char #\\newline))  
        (read-char)  
        (loop (read-char)))))  
  
(define (handle-unknown-char char)  
  (error (format "Unknown character: ~a" char)))  
  
(define (scan-file filename)  
  (with-handlers (\[exn:fail? (lambda (exn) (display (exn-message exn)) (newline))\])  
    (let (\[tokens (tokenize-file filename)\])  
      (pretty-print tokens)  
      (newline))))  
  
(scan-file "Fall24SampleCode.txt")  
  
  
  
Make a parser work the the scanner

## AI answer
To make a parser work with the scanner you've provided, we need to integrate the scanning function with the parsing functions. Here's how we can modify the existing parser to work with your scanner:

```lisp
#lang racket

;; Include your existing scanner code here
;; (define (tokenize-file filename) ...)
;; ... (all other scanner functions) ...

;; Parser functions (modified from the previous implementation)

(define (parse-program tokens)
  (let ([statements (let loop ([remaining-tokens tokens]
                               [statements '()])
                      (if (null? remaining-tokens)
                          (reverse statements)
                          (let-values ([(stmt rest) (parse-statement remaining-tokens)])
                            (loop rest (cons stmt statements)))))])
    `(PROGRAM ,@statements)))

;; ... (include all other parsing functions from the previous implementation) ...

;; Main parsing function
(define (parse tokens)
  (parse-program tokens))

;; Function to scan and parse a file
(define (scan-and-parse-file filename)
  (with-handlers ([exn:fail? (lambda (exn) 
                               (display (exn-message exn)) 
                               (newline))])
    (let ([tokens (tokenize-file filename)])
      (pretty-print (parse tokens))
      (newline))))

;; Test the scanner and parser
(scan-and-parse-file "Fall24SampleCode.txt")
```

This implementation combines your scanner with the existing parser. Here's what's been changed:

1.  We've included your scanner code at the beginning of the file (not shown here for brevity).
2.  We've modified the `parse-program` function to use `PROGRAM` instead of `STMT` as the top-level node, to better reflect the structure of a complete program.
3.  We've added a new function `scan-and-parse-file` that combines the scanning and parsing steps. It first tokenizes the file using your `tokenize-file` function, then passes the resulting tokens to the `parse` function.
4.  The `scan-and-parse-file` function is used to test the combined scanner and parser on the "Fall24SampleCode.txt" file.

To use this combined scanner and parser:

1.  Make sure all the scanner functions (like `tokenize-file`, `read-word`, `read-number`, etc.) are included at the top of the file.
2.  Include all the parser functions from the previous implementation.
3.  Run the `scan-and-parse-file` function with your input file as the argument.

This implementation will read the file, tokenize it using your scanner, then parse the tokens using the existing parser, and finally print the resulting abstract syntax tree [1]. Remember to adjust the error handling and output formatting as needed for your specific use case. Also, make sure that the token format produced by your scanner matches the format expected by the parser (e.g., `(keyword "IF")` instead of just `"IF"` for keywords).

---
**Sources:**
- (1) paste.txt



## User
I got  
Expected ENDWHILE

## AI answer
The error message "Expected ENDWHILE" suggests that there's an issue with the structure of the WHILE loop in the sample tokens. Let's analyze the WHILE loop structure in the given sample tokens:

```lisp
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
```

This structure looks correct. The WHILE loop has a proper beginning with "WHILE", a condition, a "DO" keyword, a body, and an "ENDWHILE" to close the loop. The error might be occurring because the parser is expecting an "ENDWHILE" token in a different place than where it actually appears in the token stream. This could happen if there's an issue with how the parser is handling nested structures or if it's not properly tracking the depth of nested statements. To resolve this issue, we can try the following:

1.  Ensure that the `parse-while-statement` function is correctly handling the "ENDWHILE" token.
2.  Check if there are any issues with nested structures in the parser.
3.  Verify that the `parse-statements` function is correctly handling the end of different statement types, including WHILE loops.

Here's a modified version of the `parse-while-statement` function that might help:

```lisp
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
```

This version explicitly checks for the "DO" keyword after the condition and provides a more specific error message if "ENDWHILE" is not found. If you're still encountering the "Expected ENDWHILE" error after making this change, it would be helpful to see the full error message and stack trace to pinpoint exactly where in the parsing process the error is occurring. Additionally, you might want to add some debug print statements in your parsing functions to track the flow of tokens and see where the parser might be deviating from the expected structure.

---
**Sources:**
- (1) paste.txt
- (2) paste.txt



## User
Expected ENDWHILE at the end of WHILE loop  
  
Its not parsing the scanner like i want it to be

## AI answer
I see. The error "Expected ENDWHILE at the end of WHILE loop" suggests that the parser is not correctly handling the end of WHILE loops. Let's modify the `parse-while-statement` function to address this issue:

```lisp
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
```

This modification ensures that:

1.  The "DO" keyword is explicitly checked after the condition.
2.  An error is raised if "ENDWHILE" is not found at the end of the loop.

Now, let's address the issue of the parser not working with the scanner as expected. We need to ensure that the token format produced by the scanner matches what the parser expects. Here's how we can modify the `scan-and-parse-file` function to work with your scanner:

```lisp
(define (scan-and-parse-file filename)
  (with-handlers ([exn:fail? (lambda (exn) 
                               (display (exn-message exn)) 
                               (newline))])
    (let ([tokens (tokenize-file filename)])
      (pretty-print (parse tokens))
      (newline))))
```

This function uses your `tokenize-file` function to get the tokens, then passes them to the `parse` function. To make sure everything works together, follow these steps:

1.  Include your scanner code (the `tokenize-file` function and its helpers) at the beginning of the file.
2.  Include all the parser functions.
3.  Add the `scan-and-parse-file` function at the end.
4.  Call `scan-and-parse-file` with your input file as the argument.

Here's a complete example of how your file structure should look:

```lisp
#lang racket

;; Include your scanner code here
(define (tokenize-file filename) ...)
;; ... (all other scanner functions) ...

;; Include all parser functions here
(define (parse-program tokens) ...)
;; ... (all other parser functions) ...

;; Main parsing function
(define (parse tokens)
  (parse-program tokens))

;; Function to scan and parse a file
(define (scan-and-parse-file filename)
  (with-handlers ([exn:fail? (lambda (exn) 
                               (display (exn-message exn)) 
                               (newline))])
    (let ([tokens (tokenize-file filename)])
      (pretty-print (parse tokens))
      (newline))))

;; Test the scanner and parser
(scan-and-parse-file "Fall24SampleCode.txt")
```

This structure should allow the scanner and parser to work together seamlessly. If you're still encountering issues, it would be helpful to see the exact output of your scanner (the tokens it produces) to ensure they match the format expected by the parser.

---
**Sources:**
- (1) paste.txt
- (2) paste.txt
- (3) paste.txt
- [(4) Parsing propositional logic in 33 lines of Racket - Micah Cantor](https://www.micahcantor.com/blog/logic-racket-parser/)
- [(5) While loops for Racket](https://docs.racket-lang.org/dyoo-while-loop/index.html)
- [(6) The While Loop in Racket - Stack Overflow](https://stackoverflow.com/questions/60420545/the-while-loop-in-racket)



## User
Welcome to DrRacket, version 8.14 \[cs\].  
Language: racket, with debugging; memory limit: 512 MB.  
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
  (id "ENDWHILE")  
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
  (keyword "ENDDEF"))  
  
Expected ENDWHILE at the end of WHILE loop  
  
  
i getting not the output i wanted

## AI answer
I see the issue now. The problem is that the scanner is treating "ENDWHILE" as an identifier (id) instead of a keyword. This is causing the parser to expect an "ENDWHILE" keyword but instead finding an identifier, which leads to the error. To fix this, we need to modify the `categorize-word` function in your scanner to recognize "ENDWHILE" as a keyword. Here's how you can update that function:

```lisp
(define (categorize-word word)
  (let ([upper-word (string-upcase word)])
    (cond
      [(member upper-word '("IF" "THEN" "ELSE" "WHILE" "DO" "END" "PRINT" "INPUT" "RETURN" "ENDDEF" "ENDIF" "DEF" "OR" "AND" "NOT" "ENDWHILE"))
       `(keyword ,upper-word)]
      [else `(id ,word)])))
```

This change will ensure that "ENDWHILE" is correctly categorized as a keyword. Now, let's modify the `scan-and-parse-file` function to print both the tokens and the parsed result:

```lisp
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
```

This will help us see both the tokens produced by the scanner and the parsed result. After making these changes, try running your `scan-and-parse-file` function again with your input file. The output should now correctly recognize "ENDWHILE" as a keyword, and the parser should be able to handle it properly. If you're still encountering issues, please run the updated code and share the new output, including both the tokens and the parsed result. This will help us identify any remaining problems in either the scanner or the parser.

---
**Sources:**
- (1) paste.txt
- (2) paste.txt
- (3) paste.txt
- (4) paste.txt


