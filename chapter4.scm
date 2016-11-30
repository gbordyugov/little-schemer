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

(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

(tup+ '(1 2) '(3 4))

(tup+ '(1 2 3) '(3 4))

(define larger
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (larger (sub1 x) (sub1 y))))))

(larger 5 3)

(larger 3 5)

(define smaller
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (smaller (sub1 x) (sub1 y))))))

(smaller 3 3)
(smaller 3 5)
(smaller 5 3)

(define equal
  (lambda (x y)
    (cond
      ((smaller x y) #f)
      ((larger x y)  #f)
      (else #t))))

(equal 3 3)
(equal 5 3)
(equal 3 5)

(define expo
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (mul x (expo x (sub1 y)))))))

(expo 2 0)
(expo 2 1)
(expo 2 8)

(define div
  (lambda (x y)
    (cond
      ((smaller x y) 0)
      (else (add1 (div (sub x y) y))))))

(div 9 3)
(div 9 2)
(div 15 4)

(define len
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (len (cdr l)))))))

(len '(1 2 3))
(len '())

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 1 '(1 2 3))
(pick 2 '(1 2 3))
(pick 3 '(1 2 3))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))

(define no-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (no-nums (cdr l)))
      (else (cons (car l) (no-nums (cdr l)))))))

(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (cons (car l) (all-nums (cdr l))))
      (else (all-nums (cdr l))))))

(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))

(eqan?  5  3)
(eqan?  5  5)
(eqan?  5 'a)
(eqan? 'a  5)
(eqan? 'a 'b)
(eqan? 'a 'a)

(define (occur x l)
  (cond
    ((null? l) 0)
    ((eq? (car l) x) (add1 (occur x (cdr l))))
    (else (occur x (cdr l)))))

(occur 'a '(a b a))
(occur  5 '(5 2 1))
(occur  5 '(a b c))

(define (one? x)
  (zero? (sub1 x)))

(define rempick-
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick- 2 '(hotdogs with hot mustard))
