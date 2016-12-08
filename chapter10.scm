;; let's go!

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (first x)
  (car x))

(define (second x)
  (car (cdr x)))

(define (third x)
  (car (cdr (cdr x))))

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
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values)
                                  entry-f)))))

(lookup-in-entry 'coffee '((coffee) (#t)) (lambda (x) '()))

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
  (lambda (e)
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

;;
;; don't really understand why the environment is empty here
;;
(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; continue on page 181

;;
;; action *const
;;
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

;;
;; action *quote
;;
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of
  (lambda (e)
    (second e)))

;;
;; just lookups the id in the environment
;; with initial table as the default fallback
;;
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

;;
;; actions for lambdas
;;
(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

;;
;; just to fetch the elements of a lambda
;;
(define table-of first)
(define formals-of second)
(define body-of third)


;;
;; evaluating (cond ...)
;;
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (questions-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (e)
    (cond
      (eq? e 'else))))

(define question-of first)
(define answer-of   second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#t)) ((klatsch party) (5 (6)))))

(*cond '(cond (coffee party) (else party))
       '(((coffee) (#t)) ((klatsch party) (5 (6)))))

;;
;; evaluate a list of arguments
;;
(define evlis
  (lambda (args table)
    (cond
      ((null? ) '())
      (else (cons (meaning (car args) table)
                  (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
      (meaning-of (function-of e) table)
      (evlis (arguments-of e) table))))

(define  function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitve (second fun) vals))
      ((non-primitve? fun)
       (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)    (cons    (first vals) (second vals)))
      ((eq? name 'car)     (car     (first vals)))
      ((eq? name 'cdr)     (cdr     (first vals)))
      ((eq? name 'null)    (null?   (first vals)))
      ((eq? name 'eq)      (eq?     (first vals) (second vals)))
      ((eq? name 'atom?)   (:atom?  (first vals)))
      ((eq? name 'zero?)   (zero?   (first vals)))
      ((eq? name 'add1)    (add1    (first vals)))
      ((eq? name 'sub1)    (sub1    (first vals)))
      ((eq? name 'number?) (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x)     'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))
;;
;; wow
;;
