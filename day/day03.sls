(library (day day03)
  (export get-data total-output)
  (import (rnrs base)
          (rnrs lists)
          (aoc file)
          (aoc data))

(define (get-data filename)
  (let ((char-lines (filename->char-lines filename)))
    (map list->vector 
         (map (lambda (char-line) (map list->string->num (map list char-line))) char-lines))))

(define (max-joltage bank)
  (let search ((i 1) (left-max (vector-ref bank 0)) (right-max 0) (this (vector-ref bank 1)))
    (cond
      ((= i (- (vector-length bank) 1)) (if (> this right-max)
                                          (+ (* 10 left-max) this)
                                          (+ (* 10 left-max) right-max)))
      ((> this left-max) (search (+ i 1) this 0 (vector-ref bank (+ i 1))))
      ((> this right-max) (search (+ i 1) left-max this (vector-ref bank (+ i 1))))
      (else (search (+ i 1) left-max right-max (vector-ref bank (+ i 1)))))))

(define (total-output banks)
  (fold-left + 0 (map max-joltage banks)))
)
