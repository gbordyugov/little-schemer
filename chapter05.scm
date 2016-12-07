(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(define (insertR* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l))
                (insertR* new old (cdr l))))))

(insertR* 'roast 'chuck
          '((how much (wood)) could ((a (wood) chuck))
            (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
    (else (+ (occur* a (car l)) (occur* a (cdr l))))))

(occur* 'b '((b) (b a)))

(occur* 'banana '((banana) (split ((((banana ice))) (cream (banana))
                                                    sherben))
                           (banana)
                           (bread)
                           (banana brandy)))

(define (subst* n o l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) o) (cons n (subst* n o (cdr l))))
       (else (cons (car l) (subst* n o (cdr l))))))
    (else (cons (subst* n o (car l)) (subst* n o (cdr l))))))

(subst* 'orange 'banana '((banana)
                          (split ((((banana ice)))
                                  (cream (banana))
                                  sherben))
                          (banana)
                          (bread)
                          (banana brandy)))

(define (insertL* n o l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) o) (cons n (cons o (insertL* n o (cdr l)))))
       (else (cons (car l) (insertL* n o (cdr l))))))
    (else (cons (insertL* n o (car l)) (insertL* n o (cdr l))))))

(insertL* 'pecker 'chuck
          '((how much (wood)) could ((a (wood) chuck))
            (((chuck))) (if (a) ((wood chuck))) could chuck wood))


(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
    (else (or (member* a (car l)) (member* a (cdr l))))))

(member* 'chips '(() chips ()))

(member* 'chips '((potato) (chips ((with) fish) (chips))))


(define (leftmost l)
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

(leftmost '((potato) (chips ((with) fish) (chips))))

(leftmost '(((hot) (tuna (and))) cheese))


(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))

(define (eqlist? a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((or  (null? a) (null? b)) #t)
    ((and (atom? (car a)) (atom? (car b)))
     (and (eqan? (car a) (car b)) (eqlist? (cdr a) (cdr b))))
    ((or (atom? (car a)) (atom? (car b))) #f)
    (else (and (eqlist? (car a) (car b)) (eqlist? (cdr a) (cdr b))))))

(eqlist? '(strawberry ice cream) '(strawberry ice cream))

(eqlist? '(banana ((split))) '((banana) (split)))

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))

(define (equal? a b)
  (cond
    ((and (atom? a) (atom? b)) (eqan? a b))
    ((or  (atom? a) (atom? b)) #f)
    (else (eqlist? a b))))

(define (eqlist? a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((or  (null? a) (null? b)) #f)
    (else (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b))))))

(eqlist? '(strawberry ice cream) '(strawberry ice cream))

(eqlist? '(banana ((split))) '((banana) (split)))

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))


;; continue reading page 94

(define (rember s l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((equal? (car l) s) (cdr l))
       (else (cons (car l) (rember s (cdr l))))))
    (else
      (cond
        ((equal? (car l) s) (cdr l))
        (else (cons (car l) (rember s (cdr l))))))))

(rember '(a b) '((a b) c d ((a b))))

(rember '(a b) '(((a b))))

(define (rember s l)
  (cond
    ((null? l) '())
    ((equal? (car l) s) (cdr l))
    (else (cons (car l) (rember s (cdr l))))))

(rember '(a b) '((a b) c d ((a b))))

(rember '(a b) '(((a b))))

(rember '(a) '(((a))))
