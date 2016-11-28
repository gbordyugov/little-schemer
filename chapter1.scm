(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'atom)

(atom? 'turkey)

(atom? 1492)

(atom? 'u)

(atom? '*abc$)

(list? '(atom))

(list? '(atom turkey or))

(list? '((atom turkey) or))

(car '(a b c))

(car '(((hotdogs)) (and) (pickle) relish))

(car '(((hotdogs)) (and)))

(cdr '(hamburger))

(cdr '((x) t r))

(cdr 'hotdogs)

(car (cdr '((b) (x y) ((c)))))

(cdr (cdr '((b) (x y) ((c)))))

(cons 'peanut '(butter and jelly))

(cons '(banana and) '(peanut butter and jelly))

(cons '((help) this) '(is very ((hard) to learn)))

(cons '(a b (c)) ())

(cons '((a b c)) 'b)

(cons 'a (car '((b) c d)))

(cons 'a (cdr '((b) c d)))

(atom? 'Harry)

(atom? '(Harry had a heap of apples))

(atom? (car '(Harry had a heap of apples)))

(atom? (cdr '(Harry had a heap of apples)))

(atom? (cdr '(Harry)))

(atom? (car (cdr '(swing low sweet cherry oat))))

(atom? (car (cdr '(swing (low sweet) cherry oat))))

(eq? 'Harry 'Harry)

(eq? 'margarine 'butter)

(eq? '() '(strawberry))

(eq? 6 7)

(eq? (car '(Mary had a little lamb chop)) 'Mary)

(eq? (cdr '(soured milk)) 'milk)

(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans
we need jelly beans))))

