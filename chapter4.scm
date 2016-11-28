(define add1
  (lambda (n)
    (+ n 1)))

(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)

(zero? 0)

(zero? 1492)

(+ 46 12)

(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

(o+ 3 4)
