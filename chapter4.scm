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

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add (add1 n) (sub1 m))))))

(add 3 4)

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub (sub1 n) (sub1 m))))))

(sub 4 3)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3 4))

(define mul
  (lambda (x y)
    (cond
      ((eq? y 0) 0)
      (else (add x (mul x (sub1 y)))))))

(mul 3 5)
