;;
;; Test day12
;;
(define-module (aoc y2021 day12 tests day12)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 q)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module (aoc y2021 day12 day12))

;; Test string->cave
(test-begin "day12-string->cave")
(test-equal
    "(string->cave \"ab\") => #<<cave> name: \"ab\" key: ab size: small start: #f end: #f>"
  (make-cave "ab" 'ab 'small #f #f)
  (string->cave "ab"))
(test-end)

;; Test cave-eq?
(test-begin "day12-cave-eq?")
(test-equal
    "(cave-eq? (string->cave \"ab\") (string->cave \"ab\") ) => #t"
  #t
  (cave-eq? (string->cave "ab") (string->cave "ab")))
(test-equal
    "(cave-eq? (string->cave \"ab\") (string->cave \"abc\") ) => #f"
  #f
  (cave-eq? (string->cave "ab") (string->cave "abc")))
(test-end)


;; Test cave-hash
(test-begin "day12-cave-hash")
(test-equal
    "(cave-hash (string->cave \"ab\")) => (symbol-hash 'ab)"
  (symbol-hash 'ab)
  (cave-hash (string->cave "ab")))
(test-end)

(test-begin "day12-big-cave?")
(test-equal
    "(big-cave? (string->cave \"ab\")) => #f"
  #f
  (big-cave? (string->cave "ab")))
(test-equal
    "(big-cave? (string->cave \"AB\")) => #t"
  #t
  (big-cave? (string->cave "AB")))
(test-equal
    "(big-cave? (string->cave \"start\")) => #f"
  #f
  (big-cave? (string->cave "start")))
(test-equal
    "(big-cave? (string->cave \"end\")) => #f"
  #f
  (big-cave? (string->cave "end")))
(test-end)

(test-begin "day12-small-cave?")
(test-equal
    "(small-cave? (string->cave \"ab\")) => #t"
  #t
  (small-cave? (string->cave "ab")))
(test-equal
    "(small-cave? (string->cave \"AB\")) => #f"
  #f
  (small-cave? (string->cave "AB")))
(test-equal
    "(small-cave? (string->cave \"start\")) => #f"
  #f
  (small-cave? (string->cave "start")))
(test-equal
    "(small-cave? (string->cave \"end\")) => #f"
  #f
  (small-cave? (string->cave "end")))
(test-end)

(test-begin "day12-start-cave?")
(test-equal
    "(start-cave? (string->cave \"ab\")) => #f"
  #f
  (start-cave? (string->cave "ab")))
(test-equal
    "(start-cave? (string->cave \"start\")) => #t"
  #t
  (start-cave? (string->cave "start")))
(test-equal
    "(start-cave? (string->cave \"end\")) => #f"
  #f
  (start-cave? (string->cave "end")))
(test-end)

(test-begin "day12-end-cave?")
(test-equal
    "(end-cave? (string->cave \"ab\")) => #f"
  #f
  (end-cave? (string->cave "ab")))
(test-equal
    "(end-cave? (string->cave \"start\")) => #f"
  #f
  (end-cave? (string->cave "start")))
(test-equal
    "(end-cave? (string->cave \"end\")) => #t"
  #t
  (end-cave? (string->cave "end")))
(test-end)

(test-begin "day12-parse-line")
(test-equal
    "(parse-line \"start-A\") => '(\"start\" \"A\")"
  '("start" "A")
  (parse-line "start-A"))
(test-end)

(test-begin "day12-parse-lines")
(test-equal
    "(parse-line \"start-A\") => '(\"start\" \"A\")"
  '(("start" "A"))
  (parse-lines '("start-A")))
(test-end)


(test-begin "day12-edge-eq?")
(test-equal
    "(edge-eq? (make-edge 'start 'A 'start-A) (make-edge 'start 'A 'start-A)) => #t"
  #t
  (edge-eq? (make-edge 'start 'A 'start-A) (make-edge 'start 'A 'start-A)))
(test-end)

(test-begin "day12-edge-hash")
(test-equal
    "(edge-hash (make-edge 'start 'A 'start-A) => (symbol-hash 'start-A)"
  (symbol-hash 'start-A)
  (edge-hash (make-edge 'start 'A 'start-A)))
(test-end)

(test-begin "day12-easy-make-edge")
(let ((src (string->cave "start"))
      (dst (string->cave "A")))
  (test-equal
      "(easy-make-edge src dst) => (make-edge src dst 'start-A)"
    (make-edge src dst 'start-A)
    (easy-make-edge src dst)))
(test-end)

(test-begin "day12-hashtable-copy-with-new-item")
(let ((ht (make-eq-hashtable)))
  (begin
    (hashtable-set! ht 'old-key "yay")
    (let ((new-ht (hashtable-copy-with-new-item ht 'new-key "sure")))
      (begin
	(test-equal 1 (vector-length (hashtable-keys ht)))
	(test-equal "yay" (hashtable-ref ht 'old-key #f))
	(test-equal #f (hashtable-ref ht 'new-key #f))
	(test-equal 2 (vector-length (hashtable-keys new-ht)))
	(test-equal "yay" (hashtable-ref new-ht 'old-key #f))
	(test-equal "sure" (hashtable-ref new-ht 'new-key #f))))))
(test-end)


(test-begin "day12-init")
(let ((init-data (init '("start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end"))))
  (let ((caves (first init-data))
	(edge-graph (second init-data))
	(start-cave (third init-data)))
    (begin
      (test-equal 6 (vector-length (hashtable-keys caves)))
      (test-equal 6 (vector-length (hashtable-keys edge-graph)))
      (test-equal
	  2
	  (vector-length
	   (hashtable-keys (hashtable-ref edge-graph
					  (string->cave "start") #f))))
      (test-equal
	  4
	  (vector-length
	   (hashtable-keys (hashtable-ref edge-graph
					  (string->cave "A") #f))))
      (test-equal
	  4
	  (vector-length
	   (hashtable-keys (hashtable-ref edge-graph
					  (string->cave "b") #f))))
      (test-equal
	  1
	  (vector-length
	   (hashtable-keys (hashtable-ref edge-graph
					  (string->cave "c") #f))))
      (test-equal
	  1
	  (vector-length
	   (hashtable-keys (hashtable-ref edge-graph
					  (string->cave "d") #f))))
      (test-equal
	  2
	  (vector-length
	   (hashtable-keys (hashtable-ref edge-graph
					  (string->cave "end") #f))))
      (test-equal (cave-eq? start-cave (string->cave "start")) #t))))
(test-end)

(test-begin "day12-visit-cave-part1?")
(let ((visited-caves-ht (make-hashtable cave-hash cave-eq?)))
  (begin
    (hashtable-set! visited-caves-ht (string->cave "A") #t)
    (hashtable-set! visited-caves-ht (string->cave "b") #t)
    (let ((visited-caves (make-visited-caves visited-caves-ht #f))
	  (visited-paths (make-hashtable edge-hash edge-eq?)))
      (begin
	(test-equal
	    "start cave -> (#f . #f)"
	  (cons #f #f)
	  (visit-cave-part1?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "start")))
	(test-equal
	    "end cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part1?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "end")))
	(test-equal
	    "known big cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part1?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "A")))
	(test-equal
	    "unknown big cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part1?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "C")))
	(test-equal
	    "known small cave -> (#f . #f)"
	  (cons #f #f)
	  (visit-cave-part1?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "b")))
	(test-equal
	    "unknown small cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part1?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "d")))))))
(test-end)

(test-begin "day12-visit-cave-part2?")
(let ((visited-caves-ht (make-hashtable cave-hash cave-eq?)))
  (begin
    (hashtable-set! visited-caves-ht (string->cave "A") 1)
    (hashtable-set! visited-caves-ht (string->cave "b") 1)
    (let ((visited-caves (make-visited-caves visited-caves-ht #f))
	  (visited-paths (make-hashtable edge-hash edge-eq?)))
      (begin
	(test-equal
	    "start cave -> (#f . #f)"
	  (cons #f #f)
	  (visit-cave-part2?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "start")))
	(test-equal
	    "end cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part2?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "end")))
	(test-equal
	    "known big cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part2?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "A")))
	(test-equal
	    "unknown big cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part2?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "C")))
	(test-equal
	    "known small cave one visit -> (#t . #t)"
	  (cons #t #t)
	  (visit-cave-part2?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "b")))
	(test-equal
	    "unknown small cave -> (#t . #f)"
	  (cons #t #f)
	  (visit-cave-part2?
	   visited-caves
	   visited-paths
	   (string->cave "start")
	   (string->cave "e")))))
    ;; Add another small cave that has been visited twice
    (hashtable-set! visited-caves-ht (string->cave "d") 2)
    (let ((visited-caves (make-visited-caves visited-caves-ht #t))
	  (visited-paths (make-hashtable edge-hash edge-eq?)))
      (test-equal
	  "known small cave with one visit in cave system with two small visits -> (#f . #t)"
	(cons #f #t)
	(visit-cave-part2?
	 visited-caves
	 visited-paths
	 (string->cave "start")
	 (string->cave "b")))
      (test-equal
	  "unknown small cave in cave system with two small visits -> (#t . #t)"
	(cons #t #t)
	(visit-cave-part2?
	 visited-caves
	 visited-paths
	 (string->cave "start")
	 (string->cave "e"))))))
(test-end)

;; TODO: Expand out this test a lot more
(test-begin "day12-search-helper")
(let ((init-data (init '("start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end"))))
  (let ((caves (first init-data))
	(edge-graph (second init-data))
	(start-cave (third init-data)))
    (let* ((paths (search-helper caves edge-graph start-cave visit-cave-part1?))
	   ;; TODO: Shouldn't rely on path-tree->path-list
	   (flattened-paths (path-tree->path-list paths)))
      (begin
	;; TODO: Shouldn't rely on path-tree->path-list, should test the
	;; tree itself
	(test-equal
	    10
	  (length flattened-paths))))))
(test-end)

(test-begin "day12-path-tree->path-list")
(let ((init-data (init '("start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end"))))
  (let ((caves (first init-data))
	(edge-graph (second init-data))
	(start-cave (third init-data)))
    (let* ((paths (search-helper caves edge-graph start-cave visit-cave-part1?))
	   (flattened-paths (path-tree->path-list paths)))
      (begin
	(test-equal
	    10
	  (length flattened-paths))))))
(test-end)


