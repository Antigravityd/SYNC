(use-modules (srfi srfi-1) (ice-9 pretty-print))

;; Generator candidates
(define r '((1 1) (0 1)))
(define s '((1 0) (1 1)))

;; Shorthand for the modular arithmetic operations; replace these with some n ≠ 3 to extend to Z / nZ
(define (+mod3 x y) (modulo (+ x y) 3))
(define (*mod3 x y) (modulo (* x y) 3))

;; Calculate the determinant of a 2-by-2 matrix; replace with the more complicated n-by-n version to extend beyond GL₂
(define (det matrix)
  (modulo (- (* (caar matrix) (cadadr matrix))
	     (* (cadar matrix) (caadr matrix)))
	  3))

;; Multiply two matrices (general)
(define (mmult mat1 mat2)
  (map
   (lambda (row)
     (apply map
	    (lambda column
	      (apply +mod3 (map *mod3 row column)))
	    mat2))
   mat1))



;; Take the Cartesian product of any number of lists.
(define (cart-product lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))


;; Use the Cartesian product to take the nth Cartesian power of a particular list.
;; The words of length n formed by a list of generators are precisely the nth Cartesian power of the list of generators.
(define (cart-power xs n)
  (if (= n 1)
      (map list xs)
      (cart-product (map (lambda (-) xs)
			 (iota n)))))

;; Utility function that divides a list into equal parts, e.g. (1 2 3 4) → ((1 2) (3 4)) or (1 2 3 4 5 6) → ((1 2 3) (4 5 6))
(define (chunk xs n)
  (if (null? xs)
      '()
      (let ((head (take xs n))
            (rest (drop xs n)))
        (cons head (chunk rest n)))))

;; Generate all elements of SL₂(F₃) as all 2-by-2 matrices with entries in {0, 1, 2}.
(define sl2mod3
  (filter (lambda (mat) (= (det mat) 1))
	  (map (lambda (l) (chunk l 2))
	       (cart-power '(0 1 2) 4))))


;; Recursively evaluate a word.

;; 1-words evaluate to themselves.
;; If the word's length is even, associate pairs, multiply the pairs, and evaluate the resulting word of length ½n.
;; If it's odd, multiply the first element of the word with the result of evaluating the rest.
(define (eval-word word)
  (if (= (length word) 1)
      (car word)
      (if (= (modulo (length word) 2) 0)
	  (eval-word
	   (map mmult (map car (chunk word 2)) (map cadr (chunk word 2))))
	  (mmult (car word) (eval-word (cdr word))))))




;; Given an element and a set of generators (and a cutoff length), search for a product of the generators equalling the element.

;; Starting from length-1 words, generate a list of all words from the generator.
;; Search through that list to find and return any words that evaluate to the given element; failing that, increment the length and loop.
(define (write-as-word elt gens maxlen)
  (let words ((len 1))
    (if (> len maxlen)
	#f
	(let ((match (find (lambda (word) (equal? (eval-word word) elt))
			   (cart-power gens len))))
	  (if match
	      match
	      (words (1+ len)))))))


;; Generate a pairing of elements of SL₂(F₃) with the words in terms of r and s that equal them.
(define presentation (zip sl2mod3 (map (lambda (sl2) (write-as-word sl2 (list r s) 5)) sl2mod3)))

;; Display the result of the above in a way easier on the eyes.
(do ((i 0 (1+ i)))
    ((>= i (length presentation) 1))
  (pretty-print "—————————\n" #:display? #t)
  (pretty-print (car (list-ref presentation i)) #:max-expr-width 6)
  (pretty-print "\n→\n" #:display? #t)
  (do ((j 0 (1+ j)))
      ((>= j (length (cadr (list-ref presentation i))) 1))
    (pretty-print (list-ref (cadr (list-ref presentation i)) j) #:max-expr-width 6)
    (pretty-print "\n" #:display? #t)))
