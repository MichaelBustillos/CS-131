#lang racket
(define empty '())

(define (concatenate x y) 
  (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (term-compare x y x1 y1)
    (compare_bool (recursive_bind x y x1 y1 #t)
                  (recursive_bind x y x1 y1 #f)))

(define (recursive_bind a b c lst forward) 
  (if(equal? a empty) empty
    (let ((new_binding (recursive_bind (cdr a ) (cdr b) c lst forward))
          (a1 (car a)) (b1 (car b)))
      (cond
        [(and (not (list? b1)) (list? a1)) 
          ;cons_bind(a b new_binding forward)
          (cons (if forward (compare_bool a1 b1) (compare_bool b1 a1)) new_binding)]
        [(and (not (list? a1)) (list? b1)) 
          ;cons_bind(a b new_binding forward)

          (cons (if forward (compare_bool a1 b1) (compare_bool b1 a1)) new_binding)]
        [(and (list? a1) (list? b1) (not (equal? (length a1) (length b1))))
          ;cons_bind(a b new_binding forward)

          (cons (if forward (compare_bool a1 b1) (compare_bool b1 (car a ))) new_binding)]
        [(and (list? (car a )) (or (equal? (car (car a )) 'lambda) (equal? (car (car a )) special_lambda))
          (or (equal? (car b1) 'lambda) (equal? (car b1) special_lambda)))
          (cons (if forward (expr-compare (car a ) b1) (expr-compare b1 (car a ))) new_binding)]
        [(list? a1) 
          (cons (recursive_bind a1 b1 c lst forward) new_binding)]
        [else 
          (if (member a1 lst) (cons (let ((i (elementOf a1 lst 0))) (list-ref c i)) new_binding) (cons (car a ) new_binding))]))))


(define (var_fix a b c d forward) 
  (cond
    [(and (equal? a b) (equal? a empty)) 
      (list c d forward)]
    [(equal? (car a) (car b))  
      (var_fix (cdr a) (cdr b) c d forward)]
    [else (var_fix (cdr a) (cdr b) 
      (cons (concatenate (car a) (car b)) c) (cons (car a) d) (cons (car b) forward))]))


(define (compare_letters x y)
  (cond
    [(equal? x empty) empty]
    [(equal? y empty) empty]
    [else 
      (cons (expr-compare (car x) (car y)) (compare_letters (cdr x) (cdr y)))]))

(define (cons-bind a b new_binding forward)
  (cons (if forward (compare_bool (car a ) (car b)) (compare_bool (car b) (car a ))) new_binding))


(define (helper a b)
  (cond
      [(and (list? a) (list? b) (equal? (length a) (length b))) (var_fix a b empty empty empty)]
      [(and (not (list? a)) (not (list? b))) (var_fix (cons a empty) (cons b empty) empty empty empty)]
      [else #f])
)


(define (elementOf element lst cnt) (if (equal? (car lst) element) cnt (elementOf element  (cdr lst) (+ cnt 1))))


(define (compare_lambda x y)
  (let ((a (car (cdr x))) (b (car (cdr y))))
    (let ((new_bind (helper a b)))
        (if new_bind
          (cons (if (equal? (car x) (car y)) (car x) special_lambda)
          (let ((new_x (recursive_bind (cdr x) (cdr y) (car new_bind) (list-ref new_bind 1) #t)) (new_y (recursive_bind (cdr y) (cdr x) (car new_bind) (list-ref new_bind 2) #f)))
            (expr-compare new_x new_y)))
    (compare_bool x y)))))


(define (expr-compare x y)
  (if (and (list? x) (list? y))
    (if (equal? (length x) (length y))
      (if (equal? (car x) (car y))
        (cond
          [(equal? (car x) 'quote) (compare_bool x y)]
          [(equal? (car x) 'lambda) (compare_lambda x y)]
          [(equal? (car x) special_lambda) (compare_lambda x y)]
          [else (compare_letters x y)])
        (cond
          [(equal? 'if (car x)) (compare_bool x y)]
          [(equal? 'if (car y)) (compare_bool x y)]
          [(or (and (equal? (car x) 'lambda) (equal? (car y) special_lambda))
            (and (equal? (car y) 'lambda) (equal? (car x) special_lambda))) (compare_lambda x y)]
          [else (compare_letters x y)]))
      (compare_bool x y))
    (compare_bool x y)))


(define special_lambda (string->symbol "λ"))


(define (compare_bool a b) 
  (cond
    [(equal? a b) a]
    [(and (boolean? a) (boolean? b)) 
      (if a (if b #t '%) (if b '(not %) #f)) ]
    [else (list 'if '% a b)]))


(define (test-expr-compare x y)
    (and (equal? (eval x)
                 (eval (list 'let '((% #t)) (expr-compare x y))))
         (equal? (eval y)
                 (eval (list 'let '((% #f)) (expr-compare x y))))))

(define test-expr-x
    '(list 'a
           ((lambda (a) a) 
                (cons (quote (* 7 ((lambda (a c) (f a c)) 1 2))) 
                      ((lambda (a b) 
                            (if a #f #f))) b a))
           (λ (x y) (f x y)) 3 7))

(define test-expr-y
    '(list 'b 
            ((lambda (b) b)
                (cons (quote (* 1 ((λ (a b) (f a b)) 1 2))) 
                      ((lambda (a b) 
                            (if a #t #f))) a b))
            (λ (x z) (f z x)) 3 7))


