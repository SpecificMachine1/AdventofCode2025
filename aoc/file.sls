(library
  (aoc file)
  (export file-fold filename->char-lines read-char-line)
  (import (rnrs base) (rnrs io simple) (rnrs control))
  (define (file-fold filename nil file-cons file-read finish)
    (call-with-input-file
      filename
      (lambda (port)
        (let read-loop ((file-part (file-read port)) (acc nil))
          (cond
            ((eof-object? file-part) (if finish (finish acc) acc))
            (else (read-loop (file-read port) (file-cons file-part acc))))))))

  (define (filename->char-lines filename)
    (file-fold filename '() cons read-char-line reverse))

  ;;read-line is not defined in r6rs
  (define (read-char-line port)
    (let ((char (read-char port)))
        (cond
          ((eof-object? char) char)
          ((eof-object? (peek-char port)) '())
          ((char=? #\newline char) '())
          ((char=? #\return char) (when (char=? #\newline (peek-char port))
                               (read-char port))
                             '())
          (else (cons char (read-char-line port))))))
)
