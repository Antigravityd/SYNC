(use-modules (srfi srfi-1) (ice-9 pretty-print))


;; Cartesian product of sets, implemented on lists.
(define (cart-product lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))

;; Cartesian power of a single set, implemented on lists.
;; These first two functions
(define (cart-power xs n)
  (if (= n 1)
      (map list xs)
      (cart-product (map (lambda (-) xs)
			 (iota n)))))


;; Check if the 5-tuple of the form $(a, b, c, d, n)$ representing the product
;; $(a + b\sqrt{-n})(c + d\sqrt{-n})$ has the desired form.
;; Doesn't check for square-freeness, as that'd add factorization complexity;
;; it's easy enough to do that by inspection.
(define (suitable? five)
  (and
   (= (modulo (car five) 2) 0)
   (not (= (modulo (cadr five) 2) 0))
   (= (modulo (caddr five) 2) 0)
   (not (= (modulo (cadddr five) 2) 0))
   (> (car (cddddr five)) 3)))

;; Run through the pair products in $R$ with entries smaller than ``limit''
;; that satisfy the criteria and check if they multiply to 2.
(define (check target limit start)
(filter (lambda (five)
	  (= 2
	     (- (* (car five)
		   (caddr five))
		(* (cadr five)
		   (cadddr five)
		   (car (cddddr five))))))
	(filter suitable?
		(cart-power (iota limit start) 5))))

(check 15 -2)
;; => ((2 1 4 1 6) (4 1 2 1 6) (4 1 8 5 6) (4 5 8 1 6) (8 1 4 5 6) ...)
