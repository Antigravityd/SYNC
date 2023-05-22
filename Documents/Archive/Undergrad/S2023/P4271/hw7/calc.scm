(define mu (/ (* 21 20) (+ 21 20)))
(define fac1 1.5399e11)
(define fac2 11.605)
(define resonances (map (lambda (x)
			  (list (/ (car x) 1e6)
				(/ (cadr x) 1e6)))
		    '((10 7.24e-33) (40 3.81e-15) (60 1.08e-9) (120 3.27e-4))))


(define (reaction-rate T9)
  (* (/ fac1
	(expt (* mu T9)
	      (/ 3 2)))
     (apply + (map (lambda (x)
		     (* (cadr x)
			(exp (* -1
				fac2
				(car x)
				(/ T9)))))
		   resonances))))

(reaction-rate 0.03)
;; => 282.1641000008018
(reaction-rate 0.09)
;; => 56.009257991628544
