;;
;; Test day3
;;
(define-module (aoc y2021 day3 tests day3)
  #:use-module (srfi srfi-64)
  #:use-module (aoc y2021 day3 day3))

;; ;; Test bitstring->bitvector
(test-begin "bitstring->bitvector")
(begin
  (test-equal "0 -> #*0" #*0 (bitstring->bitvector "0"))
  (test-equal "1 -> #*1" #*1 (bitstring->bitvector "1"))
  (test-equal "01 -> #*01" #*01 (bitstring->bitvector "01"))
  (test-equal "10 -> #*10" #*10 (bitstring->bitvector "10"))
  (test-equal "11 -> #*11" #*11 (bitstring->bitvector "11"))
  (test-equal "10011 -> #*10011" #*10011 (bitstring->bitvector "10011")))
(test-end)

;; ;; Test sum-bitvector-bits-in-position
(test-begin "sum-bitvector-bits-in-position")
(begin
  (test-equal "(#*01 #*11 #*00) 0 -> 1" 1
	      (sum-bitvector-bits-in-position '(#*01 #*11 #*00) 0))
  (test-equal "(#*01 #*11 #*00) 1 -> 2" 2
	      (sum-bitvector-bits-in-position '(#*01 #*11 #*00) 1)))
(test-end)

;; ;; Test rate
;; (test-begin "rate")
;; (begin
;; (test-end)

;; ;; Test gamma-rate
(test-begin "gamma-rate")
(begin
  (test-equal "(#*00 #*00) -> (#f #f)" '(#f #f) (gamma-rate '(#*00 #*00)))
  (test-equal "(#*11 #*11) -> (#t #t)" '(#t #t) (gamma-rate '(#*11 #*11)))
  ;; ties should go to #t
  (test-equal "(#*00 #*01) -> (#f #t)" '(#f #t) (gamma-rate '(#*00 #*01))))
(test-end)

;; ;; Test epsilon-rate
(test-begin "epsilon-rate")
(begin
  (test-equal "(#*00 #*00) -> (#t #t)" '(#t #t) (epsilon-rate '(#*00 #*00)))
  (test-equal "(#*11 #*11) -> (#f #f)" '(#f #f) (epsilon-rate '(#*11 #*11)))
  ;; ties should go to #f
  (test-equal "(#*00 #*01) -> (#t #f)" '(#t #f) (epsilon-rate '(#*00 #*01))))
(test-end)

;; ;; Test boolean-list->binary-string
;; (test-begin "boolean-list->binary-string")
;; (begin
;; (test-end)

;; Test parse-binary
(test-begin "parse-binary")
(begin
  (test-equal "10011 -> 19" 19 (parse-binary "10011")))
(test-end)


;; Test most-common-bit
(test-begin "most-common-bit")
(begin
  (test-equal "(#*0) 0 -> #f" #f (most-common-bit '(#*0) 0))
  (test-equal "(#*1) 0 -> #t" #t (most-common-bit '(#*1) 0))
  (test-equal "(#*0 #*0) 0 -> #f" #f (most-common-bit '(#*0 #*0) 0))
  (test-equal "(#*0 #*1) 0 -> #t" #t (most-common-bit '(#*0 #*1) 0))
  (test-equal "(#*1 #*0) 0 -> #t" #t (most-common-bit '(#*1 #*0) 0))
  (test-equal "(#*1 #*1) 0 -> #t" #t (most-common-bit '(#*1 #*1) 0))
  (test-equal "(#*00 #*00) 0 -> #f" #f (most-common-bit '(#*00 #*00) 0))
  (test-equal "(#*00 #*00) 1 -> #f" #f (most-common-bit '(#*00 #*00) 1))
  (test-equal "(#*10 #*10) 0 -> #t" #t (most-common-bit '(#*10 #*10) 0))
  (test-equal "(#*10 #*10) 1 -> #f" #f (most-common-bit '(#*10 #*10) 1)))
(test-end)

;; Test least-common-bit
(test-begin "least-common-bit")
(begin
  (test-equal "(#*0) 0 -> #t" #t (least-common-bit '(#*0) 0))
  (test-equal "(#*1) 0 -> #f" #f (least-common-bit '(#*1) 0))
  (test-equal "(#*0 #*0) 0 -> #t" #t (least-common-bit '(#*0 #*0) 0))
  (test-equal "(#*0 #*1) 0 -> #f" #f (least-common-bit '(#*0 #*1) 0))
  (test-equal "(#*1 #*0) 0 -> #f" #f (least-common-bit '(#*1 #*0) 0))
  (test-equal "(#*1 #*1) 0 -> #f" #f (least-common-bit '(#*1 #*1) 0))
  (test-equal "(#*00 #*00) 0 -> #t" #t (least-common-bit '(#*00 #*00) 0))
  (test-equal "(#*00 #*00) 1 -> #t" #t (least-common-bit '(#*00 #*00) 1))
  (test-equal "(#*10 #*10) 0 -> #f" #f (least-common-bit '(#*10 #*10) 0))
  (test-equal "(#*10 #*10) 1 -> #t" #t (least-common-bit '(#*10 #*10) 1)))
(test-end)

;; Test filter-x-bit with most-common-bit functon
(test-begin "filter-most-common-bit")
(begin
  (test-equal "(#*0) 0 -> (#*0)" '(#*0) (filter-x-bit most-common-bit '(#*0) 0))
  (test-equal "(#*1) 0 -> (#*1)" '(#*1) (filter-x-bit most-common-bit '(#*1) 0))
  (test-equal "(#*0 #*0) 0 -> (#*0 #*0)" '(#*0 #*0)
	      (filter-x-bit most-common-bit '(#*0 #*0) 0))
  (test-equal "(#*0 #*1) 0 -> (#*1)" '(#*1) (filter-x-bit most-common-bit '(#*0 #*1) 0))
  (test-equal "(#*1 #*0) 0 -> (#*1)" '(#*1) (filter-x-bit most-common-bit '(#*1 #*0) 0))
  (test-equal "(#*1 #*1) 0 -> (#*1 #*1)" '(#*1 #*1)
	      (filter-x-bit most-common-bit '(#*1 #*1) 0))
  (test-equal "(#*00 #*00) 0 -> (#*00 #*00)" '(#*00 #*00)
	      (filter-x-bit most-common-bit '(#*00 #*00) 0))
  (test-equal "(#*00 #*00) 1 -> (#*00 #*00)" '(#*00 #*00)
	      (filter-x-bit most-common-bit '(#*00 #*00) 1))
  (test-equal "(#*10 #*10) 0 -> (#*10 #*10)" '(#*10 #*10)
	      (filter-x-bit most-common-bit '(#*10 #*10) 0))
  (test-equal "(#*10 #*10) 1 -> (#*10 #*10)" '(#*10 #*10)
	      (filter-x-bit most-common-bit '(#*10 #*10) 1)))
(test-end)

;; Test filter-x-bit with least-common-bit functon
(test-begin "filter-least-common-bit")
(begin
  (test-equal "(#*0) 0 -> ()" '() (filter-x-bit least-common-bit '(#*0) 0))
  (test-equal "(#*1) 0 -> ()" '() (filter-x-bit least-common-bit '(#*1) 0))
  (test-equal "(#*0 #*0) 0 -> ()" '()
	      (filter-x-bit least-common-bit '(#*0 #*0) 0))
  (test-equal "(#*0 #*1) 0 -> (#*0)" '(#*0) (filter-x-bit least-common-bit '(#*0 #*1) 0))
  (test-equal "(#*1 #*0) 0 -> (#*0)" '(#*0) (filter-x-bit least-common-bit '(#*1 #*0) 0))
  (test-equal "(#*1 #*1) 0 -> ()" '()
	      (filter-x-bit least-common-bit '(#*1 #*1) 0))
  (test-equal "(#*00 #*00) 0 -> ()" '()
	      (filter-x-bit least-common-bit '(#*00 #*00) 0))
  (test-equal "(#*00 #*00) 1 -> ()" '()
	      (filter-x-bit least-common-bit '(#*00 #*00) 1))
  (test-equal "(#*10 #*10) 0 -> ()" '()
	      (filter-x-bit least-common-bit '(#*10 #*10) 0))
  (test-equal "(#*10 #*10) 1 -> ()" '()
	      (filter-x-bit least-common-bit '(#*10 #*10) 1)))
(test-end)
