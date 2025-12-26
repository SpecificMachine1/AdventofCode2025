(library (aoc char-list)
  (export tokenize list->string->num char-list->sexp)
  (import (rnrs base)
          (rnrs lists)
          (rnrs io simple)
          (rnrs io ports))

(define (list->string->num chars)
  (string->number (list->string chars)))

(define (tokenize char-list sep)
  (fold-right (lambda (this-char tokens)
                (if (eq? this-char sep)
                  (cons '() tokens)
                  (cons (cons this-char (car tokens)) (cdr tokens))))
              '(())
              char-list))

(define (char-list->sexp chars)
  (read (open-string-input-port (list->string (append '(#\() chars '(#\)))))))

)
