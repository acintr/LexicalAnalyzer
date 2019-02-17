#lang racket

;; ***MAIN***
(define (lexer str)
  (string-append "(" (analyze str) ")")
  )

;; ***ANALYZER***
(define (analyze str)
  (cond
    ((zero? (string-length str)) "")
    ((isNumber (car (string->list (substring str 0 1))))(string-append "(INT " (int str)))
    ((isOp (substring str 0 1)) (string-append "(OP " (op str)))
    ((isDelimiter (substring str 0 1)) (string-append "(" (delimiter str)))
    ((isID (car (string->list (substring str 0 1)))) (string-append "(ID " (identifier str)))
    ((isSpace (substring str 0 1)) (analyze (substring str 1)))
    )
  )

;; ***IDENTIFIERS***
(define (identifier str)
  (cond
    ((zero? (string-length str)) ")")
    ((isID (car (string->list (substring str 0 1)))) (string-append (substring str 0 1) (identifier (substring str 1))))
    (else (string-append ")" (analyze str)))
    )
  )

(define (isID id)
  (cond
    ((char-alphabetic? id) #T)
    (else #F)
    )
  )


;; ***INTEGERS***
(define (int str)
  (cond
    ((zero? (string-length str)) ")")
    ((isNumber(car (string->list (substring str 0 1)))) (string-append (substring str 0 1) (int (substring str 1))))
    (else (string-append ")"(analyze str)))
    )
  )

(define (isNumber i)
  (cond
    ((char-numeric? i) #T)
    ((char=? #\- i) #T)
    (else #F)
    )
  )

;; ***OPERATORS***
(define (op str)
  (cond
    ((zero? (string-length str)) ")")
    ((string=? "+" (substring str 0 1)) (string-append "+)" (analyze (substring str 1))))
    ((string=? "*" (substring str 0 1)) (string-append "*)" (analyze (substring str 1))))
    ((string=? "%"(substring str 0 1)) (string-append "module)" (analyze (substring str 1))))
    (else (string-append ")"(analyze (substring str 1))))
    )
  )

(define (isOp o)
  (cond
    ((string=? "+" o) #T)
    ((string=? "*" o) #T)
    ((string=? "%" o) #T)
    (else #F)
    )
  )

;; ***DELIMITERS***
(define (delimiter str)
  (cond
    ((zero? (string-length str)) ")")
    ((string=? "{" (substring str 0 1)) (string-append "LC)" (analyze (substring str 1))))
    ((string=? "}" (substring str 0 1)) (string-append "RC)" (analyze (substring str 1))))
    ((string=? "[" (substring str 0 1)) (string-append "LB)" (analyze (substring str 1))))
    ((string=? "]" (substring str 0 1)) (string-append "RB)" (analyze (substring str 1))))
    ((string=? "(" (substring str 0 1)) (string-append "LP)" (analyze (substring str 1))))
    ((string=? ")" (substring str 0 1)) (string-append "RP)" (analyze (substring str 1))))
    (else (string-append ")"(analyze (substring str 1))))
    )
  )

(define (isDelimiter b)
   (cond
    ((string=? "{" b) #T)
    ((string=? "}" b) #T)
    ((string=? "[" b) #T)
    ((string=? "]" b) #T)
    ((string=? "(" b) #T)
    ((string=? ")" b) #T)
    (else #F)
    )
  )

;; ***WHITESPACE***
(define (isSpace s)
  (cond
    ((string=? " " s) #T)
    ((string=? "\n" s) #T)
    ((string=? "\t" s) #T)
    (else #F)
    )
  )

