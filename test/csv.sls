;; outputs results in csv format
(library (test csv)
         (export test-start test-equal test-assert test-end)
         (import (rnrs base)
                 (rnrs io ports)
                 (rnrs io simple))
         
         (define passed 0)
         (define failed 0)
         
         (define (test-start) (display "test, \t status\n"))
         
         (define (test-equal name expected result)
           (cond
             ((equal? expected result) (set! passed (+ passed 1))
                                       (display (string-append name ", \t" "pass\n")))
             (else (set! failed (+ failed 1))
                   (display (string-append name ", \t" "fail\n")))))

         (define (test-assert name condition)
           (cond
             (condition (set! passed (+ passed 1))
                        (display (string-append name ", \t" "pass\n")))
             (else (set! failed (+ failed 1))
                   (display (string-append name ", \t" "fail\n")))))

         (define (test-end)
           (display (string-append "Passing tests: "
                                   (number->string passed)
                                   ", \tFailing tests: "
                                   (number->string failed)))
           (newline)
           (set! passed 0)
           (set! failed 0)))
