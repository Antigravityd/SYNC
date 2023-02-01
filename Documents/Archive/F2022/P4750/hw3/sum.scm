
;; Scheme Lisp
(define (seq n)
  (if (= n 0)
      1
      (* (/ 2 n)
	 (sin (/ n 2)))))

(define (sum seq start stop)
  (apply + (map seq (iota (- stop start) start))))

(sum seq -1000000 1000000)
;; => 6.283193014966864
