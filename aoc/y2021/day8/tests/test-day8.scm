;;
;; Test day8
;;
(define-module (aoc y2021 day8 tests day8)
  #:use-module (srfi srfi-64)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module ((rnrs records syntactic) :version (6))
  #:use-module (aoc y2021 day8 day8))

;; Test parse-lines
(test-begin "day8-wires")
(test-equal "(record-type? wires) => #t" #t (record-type? wires))
(test-equal "(record-type-name wires) => #t" 'wires (record-type-name wires))
(test-equal "(record-type-fields wires) => (a b c d e f g)"
  '(a b c d e f g)
  (record-type-fields wires))
(test-end)


;; TODO: Fix inconsistency with literal letters vs. characters in test
;; descriptions
;; TODO: Add more tests
(test-begin "day8-make-wires")
(let ((wires-identity (make-wires #\a #\b #\c #\d #\e #\f #\g)))
  (test-equal "(make-wires #\\a #\\b #\\c #\\d #\\e #\\f #\\g) => a = a"
    #\a (wires-a wires-identity))
  (test-equal "(make-wires #\\a #\\b #\\c #\\d #\\e #\\f #\\g) => b = b"
    #\b (wires-b wires-identity))
  (test-equal "(make-wires #\\a #\\b #\\c #\\d #\\e #\\f #\\g) => c = c"
    #\c (wires-c wires-identity))
  (test-equal "(make-wires #\\a #\\b #\\c #\\d #\\e #\\f #\\g) => d = d"
    #\d (wires-d wires-identity))
  (test-equal "(make-wires #\\a #\\b #\\c #\\d #\\e #\\f #\\g) => e = e"
    #\e (wires-e wires-identity))
  (test-equal "(make-wires #\\a #\\b #\\c #\\d #\\e #\\f #\\g) => f = f"
    #\f (wires-f wires-identity))
  (test-equal "(make-wires #\\a #\\b #\\c #\\d #\\e #\\f #\\g) => g = g"
    #\g (wires-g wires-identity)))
(let ((wires-with-false-values (make-wires #\a #\b #f #f #f #f #f)))
  (test-equal "(make-wires #\\a #\\b #f #f #f #f #f) => a = a"
    #\a (wires-a wires-with-false-values))
  (test-equal "(make-wires #\\a #\\b #f #f #f #f #f) => c = #f"
    #f (wires-c wires-with-false-values)))
(test-end)

(test-begin "day8-get-wires")
(let ((wires-identity (make-wires #\a #\b #\c #\d #\e #\f #\g)))
  (test-equal "(get-wires wires-identity '()) => '()"
    '() (get-wires wires-identity '()))
  (test-equal "(get-wires wires-identity '(#\\a)) => '(#\\a)"
    '(#\a) (get-wires wires-identity '(#\a)))
  (test-equal "(get-wires wires-identity '(#\\a)) => '(#\\b #\\a)"
    '(#\b #\a) (get-wires wires-identity '(#\b #\a))))
(test-end)

(test-begin "day8-lookup-func-builder")
(let ((wires-test (make-wires #\a #\b #f #f #f #f #f)))
  (let ((lookup-func (lookup-func-builder wires-test)))
    (test-equal "(lookup-func #\\a) => #\\a"
      #\a (lookup-func #\a))
    (test-equal "(lookup-func #\\b) => #\\b"
      #\b (lookup-func #\b))
    (test-equal "(lookup-func #\\c) => #f"
      #f (lookup-func #\c))
    (test-equal "(lookup-func #\\h) => unspecified" *unspecified* (lookup-func #\h))))
(test-end)

(test-begin "day8-wires-to-key")
(let ((wires-identity (make-wires #\a #\b #\c #\d #\e #\f #\g)))
  (let ((lookup-func (lookup-func-builder wires-identity)))
    (test-equal
	"(wires-to-key wires-identity '(#\\a) lookup-func) => 0"
      1 (wires-to-key '(#\a) lookup-func))
    (test-equal
	"(wires-to-key wires-identity '(#\\b) lookup-func) => 1"
      2 (wires-to-key '(#\b) lookup-func))
    (test-equal
	"(wires-to-key wires-identity '(#\\c) lookup-func) => 2"
      4 (wires-to-key '(#\c) lookup-func))
    (test-equal
	"(wires-to-key wires-identity '(#\\d) lookup-func) => 3"
      8 (wires-to-key '(#\d) lookup-func))
    (test-equal
	"(wires-to-key wires-identity '(#\\e) lookup-func) => 4"
      16 (wires-to-key '(#\e) lookup-func))
    (test-equal
	"(wires-to-key wires-identity '(#\\f) lookup-func) => 5"
      32 (wires-to-key '(#\f) lookup-func))
    (test-equal
	"(wires-to-key wires-identity '(#\\g) lookup-func) => 6"
      64 (wires-to-key '(#\g) lookup-func))
    (test-equal
	"(wires-to-key wires-identity '(#\\h) lookup-func) => *unspecified*"
      #f (wires-to-key '(#\h) lookup-func))))
(test-end)


(test-begin "day8-wires-to-digits")
;; Define what wires are in each digit
(let ((l '((0 . (#\a #\b #\c #\e #\f #\g))
	   (1 . (#\c #\f))
	   (2 . (#\a #\c #\d #\e #\g))
	   (3 . (#\a #\c #\d #\f #\g))
	   (4 . (#\b #\c #\d #\f))
	   (5 . (#\a #\b #\d #\f #\g))
	   (6 . (#\a #\b #\d #\e #\f #\g))
	   (7 . (#\a #\c #\f))
	   (8 . (#\a #\b #\c #\d #\e #\f #\g))
	   (9 . (#\a #\b #\c #\d #\f #\g))))
      ;; Test the simple identity map
      (wires-identity (make-wires #\a #\b #\c #\d #\e #\f #\g)))
    (test-equal
	"wires-to-digits 0 => 119"
      0
      (wires-to-digits (assq-ref l 0) wires-identity))
    (test-equal
	"wires-to-digits 1 => 36"
      1
      (wires-to-digits (assq-ref l 1) wires-identity))
    (test-equal
	"wires-to-digits 2 => 93"
      2
      (wires-to-digits (assq-ref l 2) wires-identity))
    (test-equal
	"wires-to-digits 3 => 109"
      3
      (wires-to-digits (assq-ref l 3) wires-identity))
    (test-equal
	"wires-to-digits 4 => 46"
      4
      (wires-to-digits (assq-ref l 4) wires-identity))
    (test-equal
	"wires-to-digits 5 => 107"
      5
      (wires-to-digits (assq-ref l 5) wires-identity))
    (test-equal
	"wires-to-digits 6 => 123"
      6
      (wires-to-digits (assq-ref l 6) wires-identity))
    (test-equal
	"wires-to-digits 7 => 37"
      7
      (wires-to-digits (assq-ref l 7) wires-identity))
    (test-equal
	"wires-to-digits 8 => 127"
      8
      (wires-to-digits (assq-ref l 8) wires-identity))
    (test-equal
	"wires-to-digits 9 => 111"
      9
      (wires-to-digits (assq-ref l 9) wires-identity)))
(test-end)

(test-begin "day8-list->hashtable")
(test-equal "() => empty hashtable"
  0 (hashtable-size (list->hashtable '())))
(let ((ht (list->hashtable '((#\a 5)))))
  (test-equal "((#\a 5)) => len 1"
    1 (hashtable-size ht))
  (test-equal "((#\a 5)) => #\a => 5"
    5 (hashtable-ref ht #\a #f)))
(let ((ht (list->hashtable '((#\a 5) (#\b 3)))))
  (test-equal "((#\a 5) (#\b 3)) => len 2"
    2 (hashtable-size ht))
  (test-equal "((#\a 5) (#\b 3)) => #\a => 5"
    5 (hashtable-ref ht #\a #f))
  (test-equal "((#\a 5) (#\b 3)) => #\b => 3"
    3 (hashtable-ref ht #\b #f)))
(test-end)

(test-begin "day8-functional-wires-update")
(let ((wires-identity (make-wires #\a #\b #\c #\d #\e #\f #\g)))
  (let ((new-wires (functional-wires-update wires-identity '((#\a #\c) (#\c #\a)))))
    (test-equal
	"(wires-a (functional-wires-update wires-identity '((#\\a #\\c) (#\\c #\\a))) => #\\c"
      #\c (wires-a new-wires))
    (test-equal
	"(wires-b (functional-wires-update wires-identity '((#\\a #\\c) (#\\c #\\a))) => #\\b"
      #\b (wires-b new-wires))
    (test-equal
	"(wires-c (functional-wires-update wires-identity '((#\\a #\\c) (#\\c #\\a))) => #\\a"
      #\a (wires-c new-wires))))
(test-end)

;; These tests only test the first example
(test-begin "day8-solve-a-cf")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
       (ht (index-by-number-of-wires (car parse)))
       (my-wires (make-wires #f #f #f #f #f #f #f))
       (res (solve-a-cf ht my-wires)))
  (test-equal
      "(solve-a-cf ...) => solved wires"
    (make-wires #\d #f #f #f #f #f #f)
    (car res))
  (test-equal
      "(solve-a-cf ...) => solved (a b)"
    '(#\a #\b)
    (cadr res)))
(test-end)

(test-begin "day8-solve-c-f")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
       (ht (index-by-number-of-wires (car parse)))
       (my-wires (make-wires #f #f #f #f #f #f #f))
       (res1 (solve-a-cf ht my-wires))
       (res2 (solve-c-f ht '(#\a #\b) (car res1))))
  (test-equal
      "(solve-c-f ...) => solved wires"
    (make-wires #\d #f #\a #f #f #\b #f)
    res2))
(test-end)


(test-begin "day8-solve-b")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
       (ht (index-by-number-of-wires (car parse)))
       (my-wires (make-wires #f #f #f #f #f #f #f))
       (res1 (solve-a-cf ht my-wires))
       (res2 (solve-c-f ht '(#\a #\b) (car res1)))
       (res3 (solve-b ht res2)))
  (test-equal
      "(solve-b ...) => solved wires"
    (make-wires #\d #\e #\a #f #f #\b #f)
    res3))
(test-end)

(test-begin "day8-solve-d")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
       (ht (index-by-number-of-wires (car parse)))
       (my-wires (make-wires #f #f #f #f #f #f #f))
       (res1 (solve-a-cf ht my-wires))
       (res2 (solve-c-f ht '(#\a #\b) (car res1)))
       (res3 (solve-b ht res2))
       (res4 (solve-d ht res3)))
  (test-equal
      "(solve-d ...) => solved wires"
    (make-wires #\d #\e #\a #\f #f #\b #f)
    res4))
(test-end)

(test-begin "day8-solve-g")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
       (ht (index-by-number-of-wires (car parse)))
       (my-wires (make-wires #f #f #f #f #f #f #f))
       (res1 (solve-a-cf ht my-wires))
       (res2 (solve-c-f ht '(#\a #\b) (car res1)))
       (res3 (solve-b ht res2))
       (res4 (solve-d ht res3))
       (res5 (solve-g ht res4)))
  (test-equal
      "(solve-g ...) => solved wires"
    (make-wires #\d #\e #\a #\f #f #\b #\c)
    res5))
(test-end)

(test-begin "day8-solve-e")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
       (ht (index-by-number-of-wires (car parse)))
       (my-wires (make-wires #f #f #f #f #f #f #f))
       (res1 (solve-a-cf ht my-wires))
       (res2 (solve-c-f ht '(#\a #\b) (car res1)))
       (res3 (solve-b ht res2))
       (res4 (solve-d ht res3))
       (res5 (solve-g ht res4))
       (res6 (solve-e res5)))
  (test-equal
      "(solve-e ...) => solved wires"
    (make-wires #\d #\e #\a #\f #\g #\b #\c)
    res6))
(test-end)

(test-begin "parse-line")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")))
  (test-equal
      "parse line parses correctly"
    '(("acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab") ("cdfeb" "fcadb" "cdfeb" "cdbaf"))
    parse))
(test-end)

(test-begin "index-by-number-of-wires")
(let* ((parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
       (ht (index-by-number-of-wires (car parse))))
  (test-equal
      "index has correct number of entries"
    6
    (hashtable-size ht))
  (test-equal
      "2 -> (ab)"
    '("ab")
    (hashtable-ref ht 2 #f))
  (test-equal
      "3 -> (dab)"
    '("dab")
    (hashtable-ref ht 3 #f))
  (test-equal
      "4 -> (eafb)"
    '("eafb")
    (hashtable-ref ht 4 #f))
  (test-equal
      "5 -> (fbcad gcdfa cdfbe)"
    '("fbcad" "gcdfa" "cdfbe")
    (hashtable-ref ht 5 #f))
  (test-equal
      "6 -> (cagedb cdfgeb cefabd)"
    '("cagedb" "cdfgeb" "cefabd")
    (hashtable-ref ht 6 #f))
  (test-equal
      "7 -> (acedgfb)"
    '("acedgfb")
    (hashtable-ref ht 7 #f)))
(test-end)


(test-begin "day8-solve")
(let* ((solved (solve "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")))
  (test-equal "(solve ...) => solved wires" (make-wires #\d #\e #\a #\f #\g #\b #\c) solved))
(test-end)
