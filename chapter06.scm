(define (numbered? s)
  (cond
    ((atom? s) (number? s))
    ((eq? (car (cdr s)) '+)    (and (numbered? (car           s))
                                    (numbered? (car (cdr (cdr s))))))
    ((eq? (car (cdr s)) '*)    (and (numbered? (car           s))
                                    (numbered? (car (cdr (cdr s))))))
    ((eq? (car (cdr s)) 'expt) (and (numbered? (car           s))
                                    (numbered? (car (cdr (cdr s))))))
    (else #f)))

(define (numbered? s)
  (cond
    ((atom? s) (number? s))
    (else (and (numbered? (car           s))
               (numbered? (car (cdr (cdr s))))))))

(numbered? '(2 exp (1 + 3)))

(define (value s)
  (cond
    ((number? s) s)
    ((eq? (car (cdr s)) '+)   (+   (value (car s))
                                   (value (car (cdr (cdr s))))))
    ((eq? (car (cdr s)) '*)   (*   (value (car s))
                                   (value (car (cdr (cdr s))))))
    (else (expt (value (car s))
                (value (car (cdr (cdr s))))))))

(value '(1 + 3))

(value '(1 + (2 + 1)))

(define (1st x) (cadr  x))
(define (2nd x) (caddr x))

(define (operator x)
  (car x))

(define (value s)
  (cond
    ((atom? s)                s)
    ((eq? (operator s) '+)    (+    (value (1st s)) (value (2nd s))))
    ((eq? (operator s) '*)    (*    (value (1st s)) (value (2nd s))))
    (else                     (expt (value (1st s)) (value (2nd s))))))

(value '(+ (expt 2 2) (* 3 4)))

(define sero '())

(define (sero? x)
  (null? x))

(define (edd1 x)
  (cons '() x))

(define (zub1 x)
  (cdr x))

(define (plas x y)
  (cond
    ((sero? x) y)
    (else (plas (zub1 x) (edd1 y)))))

(plas '(()()) '(()))
