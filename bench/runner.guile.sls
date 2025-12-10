(library (bench runner)
  (export start-run run milli micro nano)
  (import (guile))

(define milli 1000)
(define micro 1000000)
(define nano 1000000000)

(define (unit-name units)
  (case units
    ((1)          "seconds")
    ((1000)       "milliseconds")
    ((1000000)    "microseconds")
    ((1000000000) "nanoseconds")
    (else (string-append "1/" (number->string units) "ths of a second"))))

(define current-jiffy times)
(define (jiffies-per-second) internal-time-units-per-second)
(define flush-output-port force-output)

(define (diff t1 t2)
  (- (tms:clock t1) (tms:clock t2)))

(define-syntax time-it
  (syntax-rules ()
    ((time-it runs units body ...)
     (let ((start (current-jiffy)))
       (let lp ((i runs))
         (if (zero? i)
           (exact->inexact (* units (/ (diff (current-jiffy) start) 
                                  (jiffies-per-second)
                                  runs)))
           (begin
             body ...
             (lp (- i 1)))))))))

(define (start-run runs units)
  (display (string-append "name, "
                          (unit-name units)
                          " to run once, average "
                          (unit-name units) " for "
                          (number->string runs)
                          " runs\n"))
  (flush-output-port))

(define-syntax run
    (syntax-rules ()
      ((run name runs units body ...)
       (let ((once (time-it 1 units body ...))
             (multi (time-it runs units body ...)))
         (display (string-append name "," (number->string once) "," (number->string multi) "\n"))
         (flush-output-port)))))

)
