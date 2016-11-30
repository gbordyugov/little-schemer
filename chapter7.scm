(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a) (member? a (cdr lat)))))))

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

(define (set? x)
  (cond
    ((null? x) #t)
    ((member? (car x) (cdr x)) #f)
    (else (set? (cdr x)))))

(set? '(a b c))
(set? '(a a b c))
(set? '(apple 3 pear 4 9 apple 3 4))

(define (makeset x)
  (cond
    ((null? x) '())
    ((member? (car x) (cdr x)) (makeset (cdr x)))
    (else (cons (car x) (makeset (cdr x))))))

(makeset '(a 3 3 2 4 a sf af asdf a b))

(makeset '(apple peach pear peach plum apple lemon peach))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 3 '(1 2 3 3 2 3 4 5))

(define (makeset x)
  (cond
    ((null? x) '())
    (else (cons (car x) (makeset (multirember (car x) (cdr x)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

(makeset '(a 3 3 2 4 a sf af asdf a b))

(define (subset? s1 s2)
  (cond
    ((null? s1) #t)
    (else (and (member? (car s1) s2) (subset? (cdr s1) s2)))))

(subset? '(a b) '(a b c d))

(subset? '(a a x) '(1 2 3 a b c d))

(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

(eqset? '(a b) '(b a))
(eqset? '(3 a b) '(b a 3))
