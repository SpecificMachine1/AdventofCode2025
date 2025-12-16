(library (day day05)
  (export get-data count-fresh count-all-fresh db-join database-fresh)
  (import (rnrs base)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs sorting)
          (aoc file)
          (aoc data)
          (aoc char-list))

(define-record-type database
  (fields fresh available))

(define (get-data filename)
  (let ((tables (split-at-null (filename->char-lines filename))))
    (make-database
      (map (lambda (char-line) (map list->string->num (tokenize char-line #\-)))
           (car tables))
      (map list->string->num (cadr tables)))))
(define (db-sort db)
  (make-database (list-sort (lambda (a b) (< (car a) (car b))) (database-fresh  db))
                 (list-sort < (database-available db))))

(define (check-db db)
  (let ((sorted (db-sort db)))
    (let check-loop ((ranges (database-fresh sorted))
                     (ids (database-available sorted))
                     (fresh '())
                     (spoiled '()))
      (cond
        ((null? ids) `((fresh ,fresh) (spoiled ,spoiled)))
        ((null? ranges) `((fresh ,fresh) (spoiled ,(append (reverse ids) spoiled))))
        ((< (car ids) (caar ranges)) (check-loop ranges (cdr ids) fresh (cons (car ids) spoiled)))
        ((> (car ids) (cadar ranges)) (check-loop (cdr ranges) ids fresh spoiled))
        (else (check-loop ranges (cdr ids) (cons (car ids) fresh) spoiled))))))

(define (count-fresh db)
  (length (cadr (assq 'fresh (check-db db)))))

(define (db-join db)
  (let* ((sorted (db-sort db))
         (fresh (database-fresh sorted)))
    (let join-loop ((this (car fresh)) (ranges (cdr fresh)) (joined '()))
      (cond
        ((null? ranges) (make-database (cons this joined) (database-available db)))
        ((>= (cadr this) (caar ranges))
         (join-loop (list (car this) (max (cadr this) (cadar ranges))) (cdr ranges) joined))
        (else (join-loop (car ranges) (cdr ranges) (cons this joined)))))))

(define (count-all-fresh db)
  (fold-left (lambda (sum pair) (+ sum 1 (- (cadr pair) (car pair))))
             0
             (database-fresh (db-join db))))

)
