(use-modules (srfi srfi-1))

(define (E n)
  (- (* 1.03 n)
     (* 0.03 (expt n 2))))

(define Elist
  (map E (iota 16)))

(define (slope x x+h f{x} f{x+h})
  (/ (- f{x+h} f{x})
     (- x+h x)))

(define (deriv pairs) ;; a delightfully 80s SICP-style way to write this
  (if (< (length pairs) 2)
      '()
      (cons (slope (caar pairs)
		   (caadr pairs)
		   (cadar pairs)
		   (cadadr pairs))
	    (deriv (cdr pairs)))))

(define (Z beta Elist)
  (sum (map
	(lambda (E) (exp (* (- beta) E)))
	Elist)))

(define (zip a b)
  (if (null? a)
      '()
      (cons (cons (car a) (car b))
	    (zip (cdr a) (cdr b)))))

(define (Ebar beta Elist)
  (- (/ (Z beta Elist))))

(define (sum list)
  (fold + 0 list))

(define (avg list)
  (/ (sum list)
     (length list)))

(define (stdev list)
  (sqrt (- (expt (avg list) 2)
	   (avg (map (lambda (x) (expt x 2))
		     list)))))
