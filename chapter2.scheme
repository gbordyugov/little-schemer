(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? '(Jack Sprat could eat no chicken fat))

(lat? '((Jack) Sprat could eat no chicken fat))

(lat? '(Jack (Sprat could) eat no chicken fat))

(lat? '(bacon and eggs))

(or (null? '()) (atom? '(d e f g)))

(or (null? '(a b c)) (null? ()))

(or (null? '(a b c)) (null? '(atom)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(member? 'meat '(mashed potatoes and meat gravy))
