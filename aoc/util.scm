(define-module (aoc util)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 getopt-long)
  #:export (char->number hashtable-copy-with-new-item))

;; Somewhat saner char->number function
(define (char->number c)
  (case c
    ((#\0) 0)
    ((#\1) 1)
    ((#\2) 2)
    ((#\3) 3)
    ((#\4) 4)
    ((#\5) 5)
    ((#\6) 6)
    ((#\7) 7)
    ((#\8) 8)
    ((#\9) 9)
    (else (raise-exception
	   (make-exception-with-message (format #f "Invalid number: ~a" c))))))

;; Create an immutable copy of a R6RS hashtable with a new item
;; Returns an immutable hashtable
(define (hashtable-copy-with-new-item ht k v)
  (let ((mutable-ht (hashtable-copy ht #t)))
    (begin
      (hashtable-set! mutable-ht k v)
      (hashtable-copy mutable-ht))))
