(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


;
; took me a while to understand page 149
;

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))
      
(define eternity
  (lambda (x)
    (eternity x)))

(define (first x)
  (car x))

(define (second x)
  (car (cdr x)))

(define (build x y)
  (cons x (cons y (quote ()))))

(define shift
  (lambda (x)
    (let* ((head (car x))
           (tail (cdr x))
           (fof (car head))
           (sof (car (cdr head))))
    (build (first (first x)) (build (second (first x)) (second x))))))

(shift '((a b) c))

(shift '((a b) (c d)))

(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    (else (null? (cdr (cdr x))))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(align (build (build 1 2) (build 3 4)))

(align (build (build 1 (build 0.5 0.6)) (build 3 4)))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora)) (length* (second pora)))))))

(length* (build 1 2))

(length* (build (build 1 (build 0.5 0.6)) (build 3 4)))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* 2 (weight* (first pora))) (weight* (second pora)))))))

(weight* '((a b) c))

(weight* '(a (b c)))


(define (revpair p)
  (build (second p) (first p)))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

(shuffle '(a (b c)))

(shuffle '(a b))

; don't run it, hangs scheme48
; (shuffle '((a b) (c d)))

(define C
  (lambda (n)
    (cond
      ((eq? 1 n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (+ 1 (* 3 n)))))))

(define A
  (lambda (n m)
    (cond
      ((eq? n 0) (+ 1 m))
      ((eq? m 0) (A (- n 1) 1))
      (else (A (- n 1) (A n (- m 1)))))))

(A 1 0)

(A 1 1)

(A 2 2)

; don't run it, takes eternity to compute
; (A 4 3)

(define will-stop?
  (lambda (f)
    (cond
      (...))))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (length (cdr l)))))))
   eternity)

((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (f cdr l))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (g cdr l))))))
  eternity))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (length cdr l))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (length cdr l))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length cdr l))))))
   eternity)))

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (length (cdr l))))))))
       

((lambda (mk-length)
   (mk-length
     (mk-length
       (mk-length
         (mk-length
           eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (mk-length (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 ((mk-length mk-length) (cdr l))))))))

; not a working version

;; (((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     ((lambda (length)
;;        (lambda (l)
;;          (cond
;;            ((null? l) 0)
;;            (else (+ 1 (length (cdr l)))))))
;;      (mk-length mk-length))))
;;  '(apples))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f) (le (lambda (x) ((f f) x)))))))
