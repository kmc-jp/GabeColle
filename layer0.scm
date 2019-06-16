;; layer0 evaluator in scheme

;; 0
(define (atom x) (not (pair? x)))
(define (eq x y) (eq? x y))
(define (if3 x y z) (if x y z))
;; 1
(define true 't)
(define false #f)
(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define arity1 (cons () ()))
(define arity2 (cons () (cons () ())))
(define arity3 (cons () (cons () (cons () ()))))
(define (get-tag x) (car x))
(define (get-param x) (cadr x))
(define (get-body x) (caddr x))
;; 2
(define fold2
  (lambda (f g xs ys)
    ((lambda (fold2) (fold2 fold2 f g xs ys))
     (lambda (fold2 f g xs ys)
       ((if3 (if3 (atom xs) true (atom ys))
             (lambda () (g xs ys))
             (lambda () (f (car xs) (car ys)
                           (fold2 fold2 f g (cdr xs) (cdr ys))))))))))
(define foldr
  (lambda (f z xs)
    ((lambda (foldr) (foldr foldr f z xs))
     (lambda (foldr f z xs)
       ((if3 (atom xs)
             (lambda () z)
             (lambda () (f (car xs) (foldr foldr f z (cdr xs))))))))))
(define cps/map
  (lambda (f xs k)
    ((lambda (cps/map) (cps/map cps/map f xs k))
     (lambda (cps/map f xs k)
       ((if3 (atom xs)
             (lambda () (k xs))
             (lambda ()
               (f (car xs)
                  (lambda (hd)
                    (cps/map cps/map f (cdr xs)
                             (lambda (tl)
                               (k (cons hd tl)))))))))))))
;; 3
(define cps/subst
  (lambda (exp keys vals params k)
    ((lambda (cps/if exist lookup append make-quote make-lambda)
       ((lambda (cps/subst)
          (cps/subst cps/subst exp keys vals params k))
        (lambda (cps/subst exp keys vals params k)
          ((cps/if
            (lambda () (atom exp))
            (cps/if
             (lambda () (exist exp params))
             (lambda () (k exp))
             (lambda () (k (lookup exp make-quote keys vals))))
            (cps/if
             (lambda () (eq (get-tag exp) 'quote))
             (lambda () (k exp))
             (cps/if
              (lambda () (eq (get-tag exp) 'lambda))
              (lambda ()
                (cps/subst cps/subst (get-body exp) keys vals
                           (append (get-param exp) params)
                           (lambda (body)
                             (k (make-lambda (get-param exp) body)))))
              (lambda ()
                (cps/map (lambda (x k)
                           (cps/subst cps/subst x keys vals params k))
                         exp k)))))))))
     ;; cps/if
     (lambda (x y z)
       (lambda () ((if3 (x) y z))))
     ;; exist
     (lambda (x ys)
       (foldr (lambda (p r) (if3 (eq x p) true r)) false ys))
     ;; lookup
     (lambda (x f ks vs)
       (fold2 (lambda (k v r)
                ((if3 (eq k x) (lambda () (f v)) (lambda () r))))
              (lambda (k v) x) ks vs))
     ;; append
     (lambda (xs ys) (foldr cons ys xs))
     ;; make-quote
     (lambda (x) (cons 'quote (cons x ())))
     ;; make-lambda
     (lambda (params body)
       (cons 'lambda (cons params (cons body ()))))
     )))
(define same-len
  (lambda (xs ys)
    (fold2 (lambda (x y r) r)
           (lambda (x y) (if3 (eq x ()) (eq y ()) false))
           xs ys)))
(define cps/eval-switch
  (lambda (f k/then k/else)
    (lambda (x skip)
      ((if3 (f x) k/then k/else) x skip))))
;; 4
(define cps/eval-map
  (lambda (eval next)
    (lambda (x skip)
      (cps/map eval x
               (lambda (x) (next x skip))))))
(define cps/eval-subst
  (lambda (next)
    (lambda (x skip)
      (cps/subst (get-body (car x)) (get-param (car x)) (cdr x) ()
                 (lambda (x) (next x skip))))))
(define cps/eval-fn
  (lambda (tag ar next f)
    (cps/eval-switch
     (lambda (x) (eq (get-tag x) tag))
     (cps/eval-switch
      (lambda (x) (same-len (cdr x) ar))
      (lambda (x skip) (skip (print tag (f (cdr x)))))
      (error tag))
     next)))
;; 5
(define cps/eval-self
  (lambda (next)
    (cps/eval-switch atom
                     (lambda (x skip) (skip (print 'eval-self x)))
                     next)))
(define cps/eval-quote
  (lambda (next)
    (cps/eval-fn 'quote arity1 next car)))
(define cps/eval-lambda
  (lambda (next)
    (cps/eval-fn 'lambda arity2 next
                 (lambda (args) (cons 'lambda args)))))
(define cps/eval-atom
  (lambda (next)
    (cps/eval-fn 'atom arity1 next
                 (lambda (args) (if3 (atom (car args)) true ())))))
(define cps/eval-eq
  (lambda (next)
    (cps/eval-fn 'eq arity2 next
                 (lambda (args) (if3 (eq (car args) (cadr args)) true ())))))
(define cps/eval-car
  (lambda (next)
    (cps/eval-fn 'car arity1 next
                 (lambda (args) ((if3 (atom (car args))
                                      (lambda () ())
                                      (lambda () (car (car args)))))))))
(define cps/eval-cdr
  (lambda (next)
    (cps/eval-fn 'cdr arity1 next
                 (lambda (args) ((if3 (atom (car args))
                                      (lambda () ())
                                      (lambda () (cdr (car args)))))))))
(define cps/eval-cons
  (lambda (next)
    (cps/eval-fn 'cons arity2 next
                 (lambda (args) (cons (car args) (cadr args))))))
(define cps/eval-if
  (lambda (next)
    (cps/eval-fn 'if arity3 next
                 (lambda (args) (if3 (eq (car args) ()) (caddr args) (cadr args))))))
(define cps/eval-apply
  (lambda (next)
    (cps/eval-switch
     (lambda (x) (same-len (car x) arity3))
     (cps/eval-switch
      (lambda (x) (eq (get-tag (car x)) 'lambda))
      (cps/eval-switch
       (lambda (x) (same-len (get-param (car x)) (cdr x)))
       (cps/eval-subst
        (lambda (x skip) (next (print 'apply x) skip)))
       (error 'arity-apply))
      (error 'no-lambda))
     (error 'arity-lambda))))
;; 6
(define (cps/eval x k)
  (print (counter/get) x)
  ((if3
    (counter)
    (lambda ()
      ((cps/eval-self
        (cps/eval-quote
         (cps/eval-lambda
          (cps/eval-map
           cps/eval
           (cps/eval-switch
            (lambda (x) (atom (car x)))
            (cps/eval-atom
             (cps/eval-eq
              (cps/eval-car
               (cps/eval-cdr
                (cps/eval-cons
                 (cps/eval-if
                  (error 'unknown-fn)))))))
            (cps/eval-apply cps/eval))))))
       x k))
    (lambda () (k x)))))

;; debug
(define debug #t)
(define (toggle-debug)
  (set! debug (not debug)))
(define (print tag x)
  (if debug (begin (display tag) (display ": ") (display x) (newline) x)
      x))
(define (error reason)
  (lambda (x skip)
    (print "ERROR" reason)
    (print "value" x)))

;; misc
(define counter/value 0)
(define (counter/get) counter/value)
(define (counter/set n) (set! counter/value n))
(define limit/value 500)
(define (limit/get) limit/value)
(define (limit/set n) (set! limit/value n))
(define (counter)
  (let ((value (counter/get)))
    (counter/set (+ 1 value))
    (< value (limit/get))))
(define (counter/reset) (counter/set 0))
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
  (counter/reset)
  (cps/eval exp (lambda (x) x)))
(define (test-eval)
  (eval (list reverse numlist)))
