(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

(define insertL (insert-g ins-l))

(define insertR (insert-g ins-r))

(insertR 'o 'n '(b b b o a a a))

(insertL 'o 'n '(b b b o a a a))

(define insertL
  (insert-g
    (lambda (o n l)
      (cons n (cons o l)))))

(define insertR
  (insert-g
    (lambda (o n l)
      (cons o (cons n l)))))

(define (subst o n l)
  (cond
    ((null? l) '())
    ((eq? (car l) o) (cons n (subst o n (cdr l))))
    (else (cons (car l) (subst o n (cdr l))))))

(subst 'o 'n '(a b c o o a b o c d))

(define subst
  (insert-g
    (lambda (o n l)
      (cons n l))))

(subst 'o 'n '(a b c o o a b o c d))

(define (yyy a l)
  ((insert-g seqrem) a #f l))

(define (seqrem o n l)
  l)

(yyy 'sausage '(pizza with sausage and bacon))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) '+)
     (+ (value (frst nexp)) (value (scnd nexp))))
    ((eq? (operator nexp) '*)
     (* (value (frst nexp)) (value (scnd nexp))))
    (else
     (expt (value (frst nexp)) (value (scnd nexp))))))

(define (operator x) (car   x))
(define (frst x)     (cadr  x))
(define (scnd x)     (caddr x))

(value '(+ 1 (* 2 3)))

(define (atom-to-function x)
  (case x
    ((+)  +)
    ((*)  *)
    (else expt)))

(atom-to-function '+)

(case 3 
  (3 'three)
  ((1 3 5) 'ungerade)
  (else    'gerade))

(atom-to-function (operator '(+ 5 3)))    

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    (else ((atom-to-function (operator nexp))
           (value (frst nexp)) (value (scnd nexp))))))

(value '(+ 1 (* 2 3)))

(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))

(multirember 'a '(ad afd a a adsf a qera a d))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)  ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))

(eq?-tuna 'tuna)

(eq?-tuna 'buna)

(define multiremberT
  (lambda (test?)
    (lambda (lat)
      (cond
        ((null? lat) '())
        ((test? (car lat))    ((multiremberT test?) (cdr lat)))
        (else (cons (car lat) ((multiremberT test?) (cdr lat))))))))

((multiremberT eq?-tuna) '(shrimp salad tuna salad and tuna))

(define (multirember&co a lat col)
  (cond
    ((null? lat) (col '() '()))
    ((eq? (car lat) a)
     (multirember&co a (cdr lat) (lambda (newlat seen)
                                   (col newlat (cons (car lat) seen)))))
    (else (multirember&co a (cdr lat) (lambda (newlat seen)
                            (col (cons (car lat) newlat) seen))))))

(define (a-friend x y)
  (null? y))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

(multirember&co 'tuna '(tuna) a-friend)

(define (new-friend newlat seen)
  (a-friend newlat (cons 'tuna seen)))

;; continue reading by recapping from page 137 on
