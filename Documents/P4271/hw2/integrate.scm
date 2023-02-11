;; All spicy Legendre polynomial and weight computation tricks courtesy Bogaert et. al. 2012

;; Recursion relation for the polynomials themselves
(define (legendre-poly l x)
  (cond
   ((= l 0) 1)
   ((= l 1) x)
   (else (/ (-
	     (*
	      (1+ (* 2 l))
	      x
	      (legendre-poly (- l 1) x))
	     (* l
		(legendre-poly (- l 2) x)))
	    (1+ l)))))

;; Analytical recursion relation for $P_l'(x)$
(define (legendre-poly-deriv l x)
  (cond
   ((= l 0) 0)
   ((= l 1) 1)
   (else (/
	  (-
	   (* l (legendre-poly (- l 1) x))
	   (* l x (legendre-poly l x)))
	  (- 1 (pow x 2))))))

;; An extremely accurate starting value for computing $x_{l, k}$ where $P_l(x_{l, k}) = 0$
(define (zero-guess l k)
  (* pi
     (/ (+ (* 4 k) 3)
	(+ (* 4 l) 2))))

;; Refine the guess above---numerical paper promises it's very few iterations 'till machine $\epsilon$
(define (legendre-newton-raphson l guess tol)
  (if (<= ((legendre-poly l guess) tol)
      guess
      (newton-raphson l (- guess
			   (/ (legendre-poly l guess)
			      (legendre-poly-deriv l guess)))))))

;; Compute the $k$th zero of $P_l$
(define (legendre-zero l k)
  (legendre-newton-raphson l (zero-init-guess l k) 1e-20))

;; Compute the $k$th quadrature weight for $P_l$
(define (quadrature-weight l k)
  (/ (* 2 (- 1 (pow x 2)))
     (pow (* (1+ l)
	     (legendre-poly
	      (1+ l)
	      (legendre-zero l k)))
	  2)))

;; Change-of-variables to rescale $\int_a^b$ to $\int_{-1}^1$
(define (change-of-interval f a b)
  (lambda (x)
    (let ((mid . (/ (- b a) 2))
	  (avg . (/ (+ b a) 2)))
      (*
       (f (+ (* mid x) avg))
       mid))))

;; Actually computing the Gaussian quadrature $\sum_{i = 1}^n w_i f(x_{n, i})$
(define (gaussian-quad f n a b)
  (apply + (map *
		(map
		 (lambda (k)
		   (quadrature-weight n k))
		 (iota 1 n))
		(map
		 (lambda (k)
		   ((change-of-interval f a b)
		    (legendre-zero n k))
		 (iota 1 n))))))

(define (integrand x)
  (/ (+ (pow x 2) x)
     (1- (exp x))))

(gaussian-quad integrand 200 0 3c/2)

;; =>
