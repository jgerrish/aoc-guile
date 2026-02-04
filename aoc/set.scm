;;
;; A basic mutable set class and module
;;
;; It uses a R6RS hash table underneath
;;
;; This is the first use of GOOPS in this AOC project.  GOOPS is the
;; object oriented extension to Guile.
;;
;; GOOPS is a very powerful OO system similar to CLOS, the Common Lisp
;; Object System.  It provides a powerful system of hooks and event
;; points in object creation and initialization, member variable
;; (slot) access and other actions.  Many of these are part of the
;; Metaobject Protocol.
;;
;; The Guile info page provides a comprehensive guide to using GOOPs.
;;
;;
;; set implementation
;;
;; Items added to the set are hashed with equal-hash.  The hash is
;; used as the key in the hash table and the item itself is stored as
;; the value.
;;
;; set interface
;;
;; Supports the following methods:
;;   contains? size add! remove! set->list
;;
;; To create a set:
;;   (make <set>)
;; To make a set with initial data:
;;   (make <set> #:init-data '((a 1) (b 2)))
;;
;; To see if it contains an item:
;;   (make <set> #:init-data '((a 1) (b 2)))
;;  (contains? set '(a 1) = #t
;;  (contains? set '(b 1) = #f
;;
;; To get the size (number of items in the set):
;;   (make <set> #:init-data '((a 1) (b 2)))
;;   (size set) => 2
;;
;; To add an item:
;;   (make <set> #:init-data '((a 1) (b 2)))
;;   (add! set '(c 3))
;;
;; To remove an item:
;;   (make <set> #:init-data '((a 1) (b 2)))
;;   (remove! set '(a 1))
;;
;; To get the items as a list (items may be in any order):
;;   (make <set> #:init-data '((a 1) (b 2)))
;;   (set->list set) => '((b 2) (a 1))
;;
(define-module (aoc set)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:export (<set> contains? size add! remove! set->list))

(define-class <set> ()
  (init-data #:init-value '()
	     #:getter get-init-data
	     #:setter set-init-data!
	     #:init-keyword #:init-data)
  (ht #:init-thunk make-eq-hashtable
      #:getter get-ht
      #:setter set-ht!
      #:init-keyword #:ht))

(define-method (initialize (set <set>) . initargs)
  (begin
    (next-method)
    (let* ((init-data (get-init-data set))
	   (ht (get-ht set)))
      (for-each
       (lambda (item)
	 (let ((key (equal-hash item)))
	   (hashtable-set! ht key item)))
       init-data))))

;; Test if an item is in the set
(define-method (contains? (set <set>) item)
  (let ((ht (get-ht set))
	(key (equal-hash item)))
    (hashtable-contains? ht key)))

;; Get the number of items in the set
(define-method (size (set <set>))
  (let ((ht (get-ht set)))
    (hashtable-size ht)))

;; Add an item to the set
;;
;; If the item is new and unique, it is added.
;; If the item already exists in the set, nothing changes.
;;
;; Returns the set
(define-method (add! (set <set>) item)
  (let ((ht (get-ht set))
	(key (equal-hash item)))
    (begin
      (hashtable-set! ht key item)
      set)))

;; Remove an item from the set
;; If the item is not in the set, nothing is done
;;
;; Returns the set
(define-method (remove! (set <set>) item)
  (let ((ht (get-ht set))
	(key (equal-hash item)))
    (begin
      (hashtable-delete! ht key)
      set)))

;; Return all the items in the set as a list
(define-method (set->list (set <set>))
  (let ((ht (get-ht set)))
    (receive  (keys entries)
	(hashtable-entries ht)
      (vector->list entries))))
