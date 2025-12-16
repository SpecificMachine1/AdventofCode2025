(library (day day03)
  (export get-data total-output total-overide-output)
  (import (rnrs base)
          (rnrs lists)
          (aoc file)
          (aoc char-list)
          (aoc data))

;; did a numerical conversion for part one but for part two thought it would be easier to do
;; conversion after digits were assembled, and I think char comparison is just number comparison
(define (get-data filename)
  (let ((char-lines (filename->char-lines filename)))
    (map list->vector char-lines)))

(define (max-joltage bank)
  (let search ((i 1) (left-max (vector-ref bank 0)) (right-max #\0) (this (vector-ref bank 1)))
    (cond
      ((= i (- (vector-length bank) 1)) (if (char>? this right-max)
                                          (list->string->num (list left-max this))
                                          (list->string->num (list left-max right-max))))
      ((char>? this left-max) (search (+ i 1) this #\0 (vector-ref bank (+ i 1))))
      ((char>? this right-max) (search (+ i 1) left-max this (vector-ref bank (+ i 1))))
      (else (search (+ i 1) left-max right-max (vector-ref bank (+ i 1)))))))

(define (total-output banks)
  (fold-left + 0 (map max-joltage banks)))

(define (index-of-max-in-range bank start end)
  (let search-loop ((i start) (max-index #f) (max-value #\0) (this (vector-ref bank start)))
    (cond
      ((char=? this #\9) i) ;; don't know if overall this is a plus or minus
      ((= i end) (if (char>? this max-value) i max-index))
      ((char>? this max-value) (search-loop (+ i 1) i this (vector-ref bank (+ i 1))))
      (else (search-loop (+ i 1) max-index max-value (vector-ref bank (+ i 1)))))))

(define (safety-overide-joltage bank count)
  (let ((batteries (make-vector count))
        (bank-len (vector-length bank)))
    (let fill ((i 0) (start 0) (left (-  count 1)) (end (- bank-len count 1)))
      (cond
        ((= i count) (string->number (list->string (vector->list batteries))))
        (else (let ((this (index-of-max-in-range bank start end)))
                (vector-set! batteries i (vector-ref bank this))
                (fill (+ i 1) (+ this 1) (- left 1) (- bank-len left))))))))

(define (total-overide-output banks)
  (fold-left + 0 (map (lambda (bank) (safety-overide-joltage bank 12)) banks)))

)
