(use-modules (srfi srfi-1)
	     (srfi srfi-8))

(define (f x)
  (/ (expt x 2)
     (- (exp x) 1)))

(define (simpstep f a b)
  (* (/ (- b a) 6)
     (+ (f a)
	(f b)
	(* 4 (f (/ (+ a b) 2))))))

(define (simpson f a b n)
  (let ((step (/ (- b a) n)))
    (receive (as bs)
	     (unzip2 (unfold (lambda (x) (>= x b))
			     (lambda (x) (list x (+ x step)))
			     (lambda (x) (+ x step))
			     a))
      (fold + 0 (map (lambda (x y) (simpstep f x y))
		     as
		     bs)))))

(simpson f 1e-10 100 1200000)
