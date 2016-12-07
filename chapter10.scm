;; let's go!

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (first x)
  (car x))

(define (second x)
  (car (cdr x)))

(define (build x y)
  (cons x (cons y (quote ()))))

;; type Entry = ([Keys], [Values])
(define new-entry
  (lambda (names values)
    (build names values)))

(new-entry '(a b c) '(1 2 3))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? name) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values)
                                  entry-f)))))

;; type Table = [Entry]
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
        (lookup-in-entry name (car table)
                         (lambda (name)
                           (lookup-in-table name (cdr table)
                                            table-f)))))))
                  
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (a)
    (cond
      ((number? e)      *const)
      ((eq? e #t)       *const)
      ((eq? e #f)       *const)
      ((eq? e 'cons)    *const)
      ((eq? e 'car)     *const)
      ((eq? e 'cdr)     *const)
      ((eq? e 'null?)   *const)
      ((eq? e 'eq?)     *const)
      ((eq? e 'atom?)   *const)
      ((eq? e 'zero?)   *const)
      ((eq? e 'add1)    *const)
      ((eq? e 'sub1)    *const)
      ((eq? e 'number?) *const)
      (else             *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote)  *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond)   *cond)
         (else *aplication))
       (else *application)))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; continue on page 181
