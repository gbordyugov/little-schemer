(+ 3 2)

(define (rember-f test? a l)
  (cond
    ((null? l) '())
    ((test? (car l) a) (rember-f test? a (cdr l)))
    (else (cons (car l) (rember-f test? a (cdr l))))))

(rember-f eq? 'a '(b c a d))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(eq?-salad 'salad)

(eq?-salad 'no-salad)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) ((rember-f test?) a (cdr l)))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))

((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))

((rember-f eq?) 'tuna '(shrimp salad and tuna salad))

(define insertL-f
  (lambda (test?)
    (lambda (o n l)
      (cond
        ((null? l) '())
        ((test? (car l) o)
         (cons n (cons o ((insertL-f test?) o n (cdr l)))))
        (else (cons (car l) ((insertL-f test?) o n (cdr l))))))))

((insertL-f eq?) 'o '! '(n o p q r))

(define insertR-f
  (lambda (test?)
    (lambda (o n l)
      (cond
        ((null? l) '())
        ((test? (car l) o)
         (cons o (cons n ((insertR-f test?) o n (cdr l)))))
        (else (cons (car l) ((insertR-f test?) o n (cdr l))))))))

(define tak (insertR-f eq?))
(tak 'o '! '(n p q r))
((insertR-f eq?) 'o '! '(n o p q r))

;; continue at the bottom of page 130

(define (ins-r o n l)
  (cons o (cons n l)))

(define (ins-l o n l)
  (cons n (cons o l)))

(define insert-g
  (lambda (ins)
    (lambda (o n l)
      (cond
        ((null? l) '())
        ((eq? (car l) o) (ins o n ((insert-g ins) o n (cdr l))))
        (else (cons (car l) ((insert-g ins) o n (cdr l))))))))

((insert-g ins-r) 'o 'n '(b b b o a a a))

((insert-g ins-l) 'o 'n '(b b b o a a a))
