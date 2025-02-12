(define-module (aoc port)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:export (get-lines file-get-string-all strings-to-numbers))

;; Get lines from a file, returning a list of the lines
(define (get-lines filename)
  (let ((input-file (open-input-file filename)))
    (let reader ((lines '()))
      (let ((line (read-line input-file)))
	(begin
	  ;; (display (format #f "line: ~a\n" line))
	  (if (not (eof-object? line))
	      (begin
		(reader (cons line lines)))
	      (reverse lines)))))))

;; Return all the contents of a text file as a string
(define (file-get-string-all filename)
  (let ((input-file (open-input-file filename)))
    (get-string-all input-file)))

;; Convert a list of strings to a list of numbers
(define (strings-to-numbers lst)
  (map string->number lst))

