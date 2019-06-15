;; layer0 evaluator in scheme

;; debug
(define (print tag x) (display tag) (display ": ") (display x) (newline) x)
(define (error reason x)
  (lambda ()
    (print "ERROR" reason)
    (print "value" x)))
(define (log tag k)
  (lambda (x) (k (print tag x))))

;; util
(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define (atom x) (not (pair? x)))
(define (eq x y) (eq? x y))
(define false #f)
(define (foldr f z xs)
  (if (atom xs) z
      (f (car xs) (foldr f z (cdr xs)))))
(define (fold2 f z xs ys)
  (if (or (atom xs) (atom ys)) z
      (f (car xs) (car ys) (fold2 f z (cdr xs) (cdr ys)))))
(define (list2 x y) (cons x (cons y ())))

;; subst
(define (exist x ys)
  (foldr (lambda (p r) (or (eq x p) r)) false ys))
(define (lookup x f ks vs)
  (fold2 (lambda (k v r)
           (if (eq k x) (f v) r))
         x ks vs))
(define (append xs ys)
  (foldr cons ys xs))
(define (make-lambda params body)
  (cons 'lambda (list2 params body)))
(define (get-param x) (cadr x))
(define (get-body x) (caddr x))
(define (subst exp keys vals params)
  (cond ((atom exp)
         (if (exist exp params) exp
             (lookup exp (lambda (x) (list2 'quote x)) keys vals)))
        ((eq (car exp) 'quote) exp)
        ((eq (car exp) 'lambda)
         (make-lambda (get-param exp)
                      (subst (get-body exp) keys vals
                             (append (get-param exp) params))))
        (else
         (foldr (lambda (x r)
                  (cons (subst x keys vals params) r))
                () exp))))

;; eval
(define (cps/eval x k)
  (print counter-value x)
  (if (< counter-limit (counter)) (k x)
      (cps/pre-eval
       x k
       (lambda (x)
         (cps/map
          cps/eval (print 'map/before x)
          (lambda (x)
            (cps/post-eval
             (print 'map/after x) k
             (lambda (x)
               (cps/eval x k)))))))))
(define (cps/pre-eval x skip next)
  (cps/eval-atom x (log 'atom skip)
                 (lambda (x)
                   (cps/eval-special x (log 'special skip)
                                     next))))
(define (cps/map f xs k)
  (if (pair? xs)
      (f (car xs) (lambda (hd) (cps/map f (cdr xs)
                                        (lambda (tl)
                                          (k (cons hd tl))))))
      (k xs)))
(define (cps/post-eval x skip next)
  (let ((fn (car x)) (args (cdr x)))
    (if (atom fn)
        (cps/eval-delta fn args
                        (error 'stuck/delta x)
                        (lambda (x) (skip (print 'delta x))))
        (cps/eval-apply fn args
                        (error 'stuck/apply x)
                        (lambda (x) (next (print 'apply x)))))))
(define (cps/eval-atom x skip next)
  (if (atom x) (skip x) (next x)))
(define (cps/eval-special x skip next)
  (cond ((eq (car x) 'quote) (skip (cadr x)))
        ((eq (car x) 'lambda) (skip x))
        (else (next x))))
(define (cps/eval-delta fn args err k)
  (cond ((eq fn 'atom) (k (if (atom (car args)) 't ())))
        ((eq fn 'eq) (k (if (eq (car args) (cadr args)) 't ())))
        ((eq fn 'car) (k (if (atom (car args)) () (car (car args)))))
        ((eq fn 'cdr) (k (if (atom (car args)) () (cdr (car args)))))
        ((eq fn 'cons) (k (cons (car args) (cadr args))))
        ((eq fn 'if) (k (if (eq (car args) ()) (caddr args) (cadr args))))
        (else (err))))
(define (cps/eval-apply fn args err k)
  (if (eq? (car fn) 'lambda)
      (k (subst (get-body fn) (get-param fn) args ()))
      (err)))

;; misc
(define counter-value 0)
(define counter-limit 500)
(define (counter)
  (let ((value counter-value))
    (set! counter-value (+ 1 value))
    value))
(define (reset-counter) (set! counter-value 0))
(define (set-limit n) (set! counter-limit n))
(define reverse
  '(lambda (xs)
     ((lambda (f) (f f xs ()))
      (lambda (f xs ys)
        ((if (atom xs)
             (lambda () ys)
             (lambda () (f f (cdr xs) (cons (car xs) ys)))))))))
(define numlist
  '(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 ()))))))
(define (eval exp)
  (reset-counter)
  (cps/eval exp (lambda (x) x)))
(define (test-eval)
  (eval (list reverse numlist)))
