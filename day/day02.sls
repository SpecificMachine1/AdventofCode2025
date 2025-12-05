(library (day day02)
   (export get-data invalid-ids invalid-ids-numeric invalid-all-reps)
   (import (rnrs base)
           (rnrs lists)
           (aoc file)
           (aoc data)
           (aoc char-list))

   (define (get-data filename)
     (let ((chars (car (filename->char-lines filename)))) ;;only one line
       (define (list->string->num char-list)
         (string->number (list->string char-list)))
       (map (lambda (range-chars) (map list->string->num (tokenize range-chars #\-)))
         (tokenize  chars #\,))))

   (define range-start caar)
   (define range-end cadar)

   (define (invalid-ids ranges)
     (let range-loop ((left ranges) (acc '()))
       (if (null? left)
         acc
         (let check-loop ((nums (inclusive-range (range-start left) (range-end left)))
                          (check-acc '()))
           (cond
             ((null? nums) (range-loop (cdr left) (append check-acc acc)))
             (else (let* ((num-string (number->string (car nums)))
                          (num-len (string-length num-string)))
                     (cond
                       ((odd? num-len) (check-loop (cdr nums) check-acc))
                       ((equal? (substring num-string 0 (/ num-len 2))
                                (substring num-string (/ num-len 2) num-len))
                        (check-loop (cdr nums) (cons (car nums) check-acc)))
                       (else (check-loop (cdr nums) check-acc))))))))))

   (define (numeral-length n) (exact (ceiling (log n 10))))

   ;; same result, much faster than previous
   (define (invalid-ids-numeric ranges)
     (let range-loop ((to-go ranges) (acc '()))
       (if (null? to-go)
         acc
         (let check-loop ((nums (inclusive-range (range-start to-go) (range-end to-go)))
                          (check-acc '()))
           (if (null? nums)
             (range-loop (cdr to-go) (append check-acc acc))
             (let* ((num (car nums))
                    (num-size (numeral-length num))
                    (factor (if (even? num-size) (expt 10 (/ num-size 2)) #f)))
               (cond
                 ((not factor) (check-loop (cdr nums) check-acc))
                 ((= (div num factor) (mod num factor)) (check-loop (cdr nums) (cons num check-acc)))
                 (else (check-loop (cdr nums) check-acc)))))))))

   (define (invalid? n)
     (define (check-len? len)
       (let ((factor (expt 10 len)))
         (let check-loop ((rem (mod n factor)) (stem (div n factor)))
           (cond
             ((< stem factor) (= stem rem))
             ((= rem (mod stem factor)) (check-loop rem (div stem factor)))
             (else #f)))))
     (exists check-len? (factors (numeral-length n))))

   (define (invalid-all-reps ranges)
     (let range-loop ((to-go ranges) (acc '()))
       (if (null? to-go)
         acc
         (let check-loop ((nums (inclusive-range (range-start to-go) (range-end to-go)))
                          (check-acc acc))
           (cond
             ((null? nums) (range-loop (cdr to-go) check-acc))
             ((invalid? (car nums)) (check-loop (cdr nums) (cons (car nums) check-acc)))
             (else (check-loop (cdr nums) check-acc)))))))
)
