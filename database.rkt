#| Assignment 1 - Racket Query Language (due February 11, noon)

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         SELECT)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table) (first table))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table) (rest table))

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table) (- (length table) 1))

; Implementation of select
(define (select attrs table)
  (foldl (lambda (tuple result) 
           (append result (list (foldr (lambda (attr res) 
                                         (cons (select-value-from-tuple (attributes table) attr tuple) res))
                                       '() attrs)))) 
         '() table))

; Implementation of join tables


(define (rename-attributes table name) 
  (cons (foldr (lambda (attr result) (cons (string-append name "." attr) result)) '() (attributes table)) (tuples table)))

(define (cartesian-product table1 table2) 
  (cond [(empty? table1) table2]
        [(empty? table2) table1]
        [else (foldl (lambda (table1-tuple result) (foldl (lambda (table2-tuple res) (append res (list (append table1-tuple table2-tuple)))) result table2)) '() table1)]))


; join the attributes first then find the combination of tuples in the two.
(define (join-two-tables table1 table2) 
  (cond [(empty? table1) table2]
        [(empty? table2) table1]
        [else (append (cartesian-product (list (attributes table1)) (list (attributes table2))) (cartesian-product (rest table1) (rest table2)))]))
  
(define (join tables names) 
  (if (empty? tables) '() 
      (join-two-tables (rename-attributes (first tables) (first names)) (join (rest tables) (rest names)))))


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (select-value-from-tuple attrs attr tuple)
  (if (equal? (first attrs) attr) (first tuple) (select-value-from-tuple (rest attrs) attr (rest tuple))))

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#

; Starter for Part 4; feel free to ignore!
(define-syntax SELECT 
  (syntax-rules (FROM *)
    [(SELECT <attrs> FROM [<table> <name>] ...) (SELECT <attrs> FROM (join (list <table> ...) (list <name> ...)))]
    ; base case for select
    [(SELECT * FROM <table>) <table>]
    [(SELECT <attrs> FROM <table>) (select <attrs> <table>)]))


(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (void)]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (void)]))

;-------------------------------------------------------------

;for testing purpose

(define Person
  '(("Name" "Age" "LikesChocolate")
    ("David" 20 #t)
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))

