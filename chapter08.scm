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
     (multirember&co a (cdr lat)
                     (lambda (newlat seen)
                       (col newlat (cons (car lat) seen)))))
    (else (multirember&co a (cdr lat)
                          (lambda (newlat seen)
                            (col (cons (car lat) newlat) seen))))))

(define (a-friend x y)
  (null? y))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

(multirember&co 'tuna '(tuna) a-friend)

(define (new-friend newlat seen)
  (a-friend newlat (cons 'tuna seen)))

;; continue reading by recapping from page 137 on

(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old)
     (cons new (cons old (multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat))))))

(multiinsertL 'new 'old '(old old old new old))

(define (multiinsertLR new oldL oldR lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) oldL)
     (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
    ((eq? (car lat) oldR)
     (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
    (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))

(multiinsertLR 'new 'oldL 'oldR '(oldR oldL old new oldL))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
    ((null? lat) (col '() 0 0))
    ((eq? (car lat) oldL)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (la l r)
                         (col (cons new (cons oldL la)) (+ 1 l) r))))
    ((eq? (car lat) oldR)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (la l r)
                         (col (cons oldR (cons new la)) l (+ 1 r)))))
    (else
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (la l r)
                         (col (cons (car lat) la) l r))))))

(multiinsertLR&co 'cranberries 'fish 'chips '(fish chips)
                  (lambda (l x y)
                    (list l x y)))

(define (evens-only* s)
  (cond
    ((null? s) '())
    ((atom? (car s))
     (if (even? (car s))
       (cons (car s) (evens-only* (cdr s)))
       (evens-only* (cdr s))))
    (else (cons (evens-only* (car s)) (evens-only* (cdr s))))))

(evens-only* '(1 2 (1 2 3 (1 2 3 (1 2) 3) (3))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

;;
;; this one is tough
;;
(define (evens-only*&co x col)
  (cond
    ((null? x) (col '() 0 1))
    ((atom? (car x))
     (if (even? (car x))
       (evens-only*&co (cdr x) (lambda (l s p)
                                 (col (cons (car x) l) s (* p (car x)))))
       (evens-only*&co (cdr x) (lambda (l s p)
                                 (col l (+ s (car x)) p)))))
    (else (evens-only*&co (car x)
                          (lambda (al as ap)
                            (evens-only*&co (cdr x)
                                            (lambda (dl ds dp)
                                              (col (cons al dl)
                                                   (+ ap dp)
                                                   (* as ds)))))))))

(evens-only*&co '(2) (lambda (l p s) (list l p s)))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) (lambda (l p s)
                                                  (list l p s)))


