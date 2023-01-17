(define p0 '())
(define (succ n) (list n))
(define (+ a b)
  (if (equal? b p0)
      a
      (succ (+ a (car b)))))

(+ ;; 3 + 2 = 5
 '(((())))
 '((())))
