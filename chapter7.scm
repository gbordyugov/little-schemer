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

(define (intersect? s1 s2)
  (cond
    ((null? s1) #f)
    (else (or (member? (car s1) s2) (intersect? (cdr s1) s2)))))

(intersect? '(a b) '(x c))
(intersect? '(a b) '(b c d))


(define (intersect s1 s2)
  (cond
    ((null? s1) '())
    ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
    (else (intersect (cdr s1) s2))))

(intersect '(a b c) '(c d e b))


(define (union s1 s2)
  (cond
    ((null? s1) s2)
    (else (makeset (cons (car s1) (union (cdr s1) s2))))))

(define (union s1 s2)
  (cond
    ((null? s1) s2)
    ((member (car s1) s2) (union (cdr s1) s2))
    (else (cons (car s1) (union (cdr s1) s2)))))

(union '(c c a b c) '(c d e))

(define (intersect-all l)
  (cond
    ((null? (cdr l)) (car l))
    (else (intersect (car l) (intersect-all (cdr l)))))))

(intersect '(b c d) '(c d e))
(intersect-all '((c d e)))
(intersect-all '((b c d) (c d e)))
(intersect-all '((a b c) (b c d) (c d e)))

(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    (else (null? (cdr (cdr x))))))

(a-pair? 3)
(a-pair? 'a)
(a-pair? '())
(a-pair? '(a))
(a-pair? '(a b))
(a-pair? '(a b c))


(define (first x)
  (car x))

(define (second x)
  (car (cdr x)))

(define (build x y)
  (cons x (cons y (quote ())))))

(define (third x)
  (car (cdr (cdr x))))

(define (fun? x)
  (set? (firsts x)))

(define (revrel r)
  (cond
    ((null? r) '())
    (else (cons (build (second (car r)) (first (car r)))
                (revrel (cdr r))))))

(revrel '((1 2) (3 4) (5 6)))

(define (revpair p)
  (build (second p) (first p)))

(define (revrel r)
  (cond
    ((null? r) '())
    (else (cons (revpair (car r)) (revrel (cdr r))))))

(define (fulfun? f)
  (set? (seconds f)))
