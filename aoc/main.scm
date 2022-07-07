(define-module (aoc main)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:export (get-args))

(define (get-args)
  (let* ((option-spec '((input (single-char #\i) (value #t))
			(help (single-char #\h) (value #f))))
	 (options (getopt-long (command-line) option-spec))
	 (help-wanted (option-ref options 'help #f))
	 (filename (option-ref options 'input #f)))
    filename))

;; return an indexed list
;; For every item x in the list, replace it with an (index x)
;; The index starts at 0, there is no guarantee on the returned order of the list, but
;; indexes will still match the original item position
(define (indexed-list l)
  (fold
   (lambda (a prev)
     (cons (list (1+ (caar prev)) a) prev))
   (list (list 0 (car l)))
   (cdr l)))

;; this pipeline isn't efficient
;; "distribute" a number through a list
(define (distribute l)
  (map
   (lambda (x)
     (map
      (lambda (y)
	(list (car x) y))
      (cadr x)))
   l))

