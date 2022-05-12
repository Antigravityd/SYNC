(define (u x)
  (/ (/ (expt x 5))
     (- (exp (/ x))
	1)))

(define (cdiff f x h)
  (/ (- (f (+ x (/ h 2)))
	(f (- x (/ h 2))))
     h))

(define (sign x)
  (if (negative? x)
      -1
      1))

(define (bisect f interval tol)
  (let*
      ((mid (/ (+ (cadr interval) (car interval)) 2))
       (fmid (f mid)))
    (if (< (abs fmid) tol)
	(list mid fmid)
	(bisect f
		(if
		 (= (sign fmid) (sign (f (cadr interval))))
		 (list (car interval) mid)
		 (list mid (cadr interval)))
		tol))))

(bisect (lambda (x) (cdiff u x 0.001))
	'(0.1 1)
	0.00001)
