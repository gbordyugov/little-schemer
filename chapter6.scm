(define (numbered? s)
  (cond
    ((atom? s) (number? s))
    ((eq? (car (cdr s)) '+)   (and (numbered? (car           s))
                                   (numbered? (car (cdr (cdr s))))))
    ((eq? (car (cdr s)) '*)   (and (numbered? (car           s))
                                   (numbered? (car (cdr (cdr s))))))
    ((eq? (car (cdr s)) 'exp) (and (numbered? (car           s))
                                   (numbered? (car (cdr (cdr s))))))
    (else #f)))

(define (numbered? s)
  (cond
    ((atom? s) (number? s))
    (else (and (numbered? (car           s))
               (numbered? (car (cdr (cdr s))))))))

(numbered? '(2 exp (1 + 3)))
