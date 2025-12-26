(library (day day06)
  (export get-data problem-sum get-data-2)
  (import (rnrs base)
          (rnrs lists)
          (rnrs unicode)
          (rnrs io simple)
          (aoc file)
          (aoc data)
          (aoc char-list))

(define (get-data filename)
  (transpose (map char-list->sexp (filename->char-lines filename))))

(define (work-problems problems)
  (map (lambda (prob)
         (let ((op (car prob)))
           (cond
             ((eq? op '+) (apply + (cdr prob)))
             ((eq? op '*) (apply * (cdr prob))))))
       (map reverse problems)))

(define (problem-sum problems)
  (fold-left + 0 (work-problems problems)))

(define (get-data-2 filename)
  (let ((columns (transpose (filename->char-lines filename))))
    (define (space-col->null column)
      (if (for-all (lambda (char) (char=? char #\space)) column)
        '()
        column))
    (define (split-off-numbers column)
      (let-values (((num-chars op-chars) (partition char-numeric? column)))
        (cons (list->string->num num-chars) (char-list->sexp op-chars))))
    (define (column->problem column)
      (let prob-loop ((this (split-off-numbers (car column))) (rest (cdr column)) (num '()) (op #f))
        (cond
          ((null? rest) (if (not (null? (cdr this)))
                          (append (cons (car this) num) (cdr this))
                          (append (cons (car this) num) op)))
          ((null? (cdr this)) (prob-loop (split-off-numbers (car rest))
                                         (cdr rest)
                                         (cons (car this) num)
                                         op))
          (else
            (prob-loop (split-off-numbers (car rest)) (cdr rest) (cons (car this) num) (cdr this))))))
    (map column->problem (split-at-null (map space-col->null columns)))))


;(get-data-2 "./data/day06-example1.dat")

)
