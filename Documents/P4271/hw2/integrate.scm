;; All spicy Legendre polynomial and weight computation tricks courtesy Bogaert et. al. 2012

(use-modules (srfi srfi-1))
;; Recursion relation for the polynomials themselves
(define (legendre-poly l x)
  (cond
   ((= l 0) 1)
   ((= l 1) x)
   (else (/ (-
	     (*
	      (- (* 2 l) 1)
	      x
	      (legendre-poly (- l 1) x))
	     (* (- l 1)
		(legendre-poly (- l 2) x)))
	    l))))



;; Analytical recursion relation for $P_l'(x)$
(define (legendre-poly-deriv l x)
  (cond
   ((= l 0) 0)
   ((= l 1) 1)
   (else (/
	  (-
	   (* l (legendre-poly (- l 1) x))
	   (* l x (legendre-poly l x)))
	  (- 1 (expt x 2))))))



;; Refine the guess above---numerical paper promises it's very few iterations 'till machine $\epsilon$
(define (legendre-newton-raphson l guess tol)
  (if (<= (abs (legendre-poly l guess)) tol)
      guess
      (legendre-newton-raphson l
			       (- guess
				  (/ (legendre-poly l guess)
				     (legendre-poly-deriv l guess)))
			       tol)))

(legendre-newton-raphson 2 0.5 1e-15)
(legendre-poly 2 (legendre-newton-raphson 2 0.5 1e-15))

(legendre-newton-raphson 3 -0.5 1e-15)

;; Compute the zeroes of $P_l$
(define (legendre-zeros l)
  (delete-duplicates
   (map (lambda (x)
	  (legendre-newton-raphson l x 1e-8))
	(iota (* 2 l)
	      (+ -1.0 (/ 2 (- (* 2 l) 1)))
	      (/ 2 (+ (* 2 l) 2))))
   (lambda (x y) (<= (abs (- x y))
		     1e-8))))



;; Compute the quadrature weights for $P_l$
(define (quadrature-weights l)
  (map
   (lambda (x)
     (/ 2
	(*
	 (- 1 (expt x 2))
	 (expt (legendre-poly-deriv l x) 2))))
   (legendre-zeros l)))


;; Change-of-variables to rescale $\int_a^b$ to $\int_{-1}^1$
(define (change-of-interval f a b)
  (lambda (x)
    (let ((mid (/ (- b a) 2))
	  (avg (/ (+ b a) 2)))
      (*
       (f (+ (* mid x) avg))
       mid))))

;; Actually computing the Gaussian quadrature $\sum_{i = 1}^n w_i f(x_{n, i})$
(define (gaussian-quad f n a b)
  (apply + (map *
		(quadrature-weights n)
		(map
		 (change-of-interval f a b)
		 (legendre-zeros n)))))

(define (integrand x)
  (/ (+ (expt x 2) x)
     (1- (exp x))))




(gaussian-quad integrand 19 0 12)

;; =>
