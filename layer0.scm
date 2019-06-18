;; layer0 evaluator in scheme

;; debug
(define debug #t)
(define (debug/on) (set! debug #t))
(define (debug/off) (set! debug #f))
(define (debug/toggle) (set! debug (not debug)))

;; 0
(define (atom x) (not (pair? x)))
(define (eq x y) (eq? x y))
;; 1
(define true 't)
(define false #f)
(define (if3 x y z) (if x y z))
(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define arity1 (cons () ()))
(define arity2 (cons () (cons () ())))
(define arity3 (cons () (cons () (cons () ()))))
(define (get-tag x) (car x))
(define (get-param x) (cadr x))
(define (get-body x) (caddr x))
(define (print tag x)
  (if debug (begin (display tag) (display ": ") (display x) (newline) x)
      x))
(define (error reason)
  (lambda (x skip)
    (print "ERROR" reason)
    (print "value" x)))
;; 2
(define fold2
  (lambda (f g xs ys)
    ((lambda (fold2-rec) (fold2-rec fold2-rec f g xs ys))
     (lambda (fold2-rec f g xs ys)
       ((lambda (fold2)
          ((if3 (if3 (atom xs) true (atom ys))
                (lambda () (g xs ys))
                (lambda () (f (car xs) (car ys)
                              (fold2 f g (cdr xs) (cdr ys)))))))
        (lambda (f g xs ys) (fold2-rec fold2-rec f g xs ys))
        ))
     )))
(define foldr
  (lambda (f z xs)
    ((lambda (foldr-rec) (foldr-rec foldr-rec f z xs))
     (lambda (foldr-rec f z xs)
       ((lambda (foldr)
          ((if3 (atom xs)
                (lambda () z)
                (lambda () (f (car xs) (foldr f z (cdr xs)))))))
        (lambda (f z xs) (foldr-rec foldr-rec f z xs))
        ))
     )))
(define cps/map
  (lambda (f xs k)
    ((lambda (cps/map-rec) (cps/map-rec cps/map-rec f xs k))
     (lambda (cps/map-rec f xs k)
       ((lambda (cps/map)
          ((if3 (atom xs)
                (lambda () (k xs))
                (lambda ()
                  (f (car xs)
                     (lambda (hd)
                       (cps/map f (cdr xs)
                                (lambda (tl)
                                  (k (cons hd tl))))))))))
        (lambda (f xs k) (cps/map-rec cps/map-rec f xs k))
        ))
     )))
;; 3
(define cps/subst
  (lambda (exp keys vals params k)
    ((lambda (cps/subst-rec)
       (cps/subst-rec cps/subst-rec exp keys vals params k))
     (lambda (cps/subst-rec exp keys vals params k)
       ((lambda (cps/subst cps/if exist lookup append make-quote make-lambda)
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
                (cps/subst (get-body exp) keys vals
                           (append (get-param exp) params)
                           (lambda (body)
                             (k (make-lambda (get-param exp) body)))))
              (lambda ()
                (cps/map (lambda (x k)
                           (cps/subst x keys vals params k))
                         exp k)))))))
        ;; cps/subst
        (lambda (exp keys vals params k)
          (cps/subst-rec cps/subst-rec exp keys vals params k))
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
        ))
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
(define cps/eval
  (lambda (x k)
    ((lambda (cps/eval-rec) (cps/eval-rec cps/eval-rec x k))
     (lambda (cps/eval-rec x k)
       ((lambda (cps/eval start end normal unknown)
          (print (counter/get) x)
          ((if3
            (counter)
            (start
             (cps/eval-self
              (cps/eval-quote
               (cps/eval-lambda
                (cps/eval-map
                 cps/eval
                 (cps/eval-switch
                  normal
                  (cps/eval-atom
                   (cps/eval-eq
                    (cps/eval-car
                     (cps/eval-cdr
                      (cps/eval-cons
                       (cps/eval-if
                        unknown))))))
                  (cps/eval-apply cps/eval)))))))
            end
            )))
        (lambda (x k) (cps/eval-rec cps/eval-rec x k))
        (lambda (eval) (lambda () (eval x k)))
        (lambda () (k x))
        (lambda (x) (atom (car x)))
        (error 'unknown-fn)
        ))
     )))

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

(define layer0
  '((lambda (true false cadr caddr arity1 arity2 arity3 print. error)
      ((lambda (fold2 foldr cps/map)
         ((lambda (cps/subst same-len cps/eval-switch)
            ((lambda (cps/eval-map cps/eval-subst cps/eval-fn)
               ((lambda (cps/eval-self
                         cps/eval-quote cps/eval-lambda cps/eval-atom cps/eval-eq
                         cps/eval-car cps/eval-cdr cps/eval-cons cps/eval-if
                         cps/eval-apply)
                  ;; eval
                  (lambda (exp)
                    ((lambda (cps/eval) (cps/eval cps/eval exp (lambda (x) x)))
                     ;; cps/eval
                     (lambda (cps/eval x k)
                       ((cps/eval-self
                         (cps/eval-quote
                          (cps/eval-lambda
                           (cps/eval-map
                            (lambda (x k) (cps/eval cps/eval x k))
                            (cps/eval-switch
                             (lambda (x) (atom (car x)))
                             (cps/eval-atom
                              (cps/eval-eq
                               (cps/eval-car
                                (cps/eval-cdr
                                 (cps/eval-cons
                                  (cps/eval-if (error (quote unknown-fn))))))))
                             (cps/eval-apply
                              (lambda (x k) (cps/eval cps/eval x k))))))))
                        x k))
                     )))
                ;; cps/eval-self
                (lambda (next)
                  (cps/eval-switch
                   atom
                   (lambda (x skip) (skip (print (quote eval-self) x)))
                   next))
                ;; cps/eval-quote
                (lambda (next)
                  (cps/eval-fn (quote quote) arity1 next car))
                ;; cps/eval-lambda
                (lambda (next)
                  (cps/eval-fn (quote lambda) arity2 next
                               (lambda (args)
                                 (cons (quote lambda) args))))
                ;; cps/eval-atom
                (lambda (next)
                  (cps/eval-fn (quote atom) arity1 next
                               (lambda (args)
                                 (if (atom (car args)) true ()))))
                ;; cps/eval-eq
                (lambda (next)
                  (cps/eval-fn (quote eq) arity2 next
                               (lambda (args)
                                 (if (eq (car args) (cadr args)) true ()))))
                ;; cps/eval-car
                (lambda (next)
                  (cps/eval-fn (quote car) arity1 next
                               (lambda (args)
                                 ((if (atom (car args))
                                      (lambda () ())
                                      (lambda () (car (car args))))))))
                ;; cps/eval-cdr
                (lambda (next)
                  (cps/eval-fn (quote cdr) arity1 next
                               (lambda (args)
                                 ((if (atom (car args))
                                      (lambda () ())
                                      (lambda () (cdr (car args))))))))
                ;; cps/eval-cons
                (lambda (next)
                  (cps/eval-fn (quote cons) arity2 next
                               (lambda (args)
                                 (cons (car args) (cadr args)))))
                ;; cps/eval-if
                (lambda (next)
                  (cps/eval-fn (quote if) arity3 next
                               (lambda (args)
                                 (if (eq (car args) ())
                                     (caddr args)
                                     (cadr args)))))
                ;; cps/eval-apply
                (lambda (next)
                  (cps/eval-switch
                   (lambda (x) (same-len (car x) arity3))
                   (cps/eval-switch
                    (lambda (x) (eq (get-tag (car x)) (quote lambda)))
                    (cps/eval-switch
                     (lambda (x) (same-len (get-param (car x)) (cdr x)))
                     (cps/eval-subst
                      (lambda (x skip) (next (print (quote apply) x) skip)))
                     (error (quote arity-apply)))
                    (error (quote no-lambda)))
                   (error (quote arity-lambda))))
                ))
             ;; cps/eval-map
             (lambda (eval next)
               (lambda (x skip)
                 (cps/map eval x (lambda (x) (next x skip)))))
             ;; cps/eval-subst
             (lambda (next)
               (lambda (x skip)
                 (cps/subst (get-body (car x)) (get-param (car x)) (cdr x) ()
                            (lambda (x) (next x skip)))))
             ;; cps/eval-fn
             (lambda (tag ar next f)
               (cps/eval-switch
                (lambda (x) (eq (get-tag x) tag))
                (cps/eval-switch
                 (lambda (x) (same-len (cdr x) ar))
                 (lambda (x skip) (skip (print tag (f (cdr x)))))
                 (error tag))
                next))
             ))
          ;; cps/subst
          (lambda (exp keys vals params k)
            ((lambda (cps/if exist lookup append make-quote make-lambda)
               ((lambda (cps/subst) (cps/subst cps/subst exp keys vals params k))
                (lambda (cps/subst exp keys vals params k)
                  ((cps/if
                    (lambda () (atom exp))
                    (cps/if
                     (lambda () (exist exp params))
                     (lambda () (k exp))
                     (lambda () (k (lookup exp make-quote keys vals))))
                    (cps/if
                     (lambda () (eq (get-tag exp) (quote quote)))
                     (lambda () (k exp))
                     (cps/if
                      (lambda () (eq (get-tag exp) (quote lambda)))
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
             (lambda (x y z) (lambda () ((if (x) y z))))
             ;; exist
             (lambda (x ys) (foldr (lambda (p r) (if (eq x p) true r)) false ys))
             ;; lookup
             (lambda (x f ks vs) (fold2 (lambda (k v r)
                                          ((if (eq k x)
                                               (lambda () (f v))
                                               (lambda () r))))
                                        (lambda (k v) x)
                                        ks vs))
             ;; append
             (lambda (xs ys) (foldr cons ys xs))
             ;; make-quote
             (lambda (x) (cons (quote quote) (cons x ())))
             ;; make-lambda
             (lambda (params body) (cons (quote lambda) (cons params (cons body ()))))
             ))
          ;; same-len
          (lambda (xs ys) (fold2 (lambda (x y r) r)
                                 (lambda (x y) (if (eq x ()) (eq y ()) false))
                                 xs ys))
          ;; cps/eval-switch
          (lambda (f k/then k/else) (lambda (x skip)
                                      ((if (f x) k/then k/else) x skip)))
          ))
       ;; fold2
       (lambda (f g xs ys)
         ((lambda (fold2) (fold2 fold2 f g xs ys))
          (lambda (fold2 f g xs ys)
            ((if (if (atom xs) true (atom ys))
                 (lambda () (g xs ys))
                 (lambda () (f (car xs) (car ys)
                               (fold2 fold2 f g (cdr xs) (cdr ys)))))))))
       ;; foldr
       (lambda (f z xs)
         ((lambda (foldr) (foldr foldr f z xs))
          (lambda (foldr f z xs)
            ((if (atom xs)
                 (lambda () z)
                 (lambda () (f (car xs) (foldr foldr f z (cdr xs)))))))))
       ;; cps/map
       (lambda (f xs k)
         ((lambda (cps/map) (cps/map cps/map f xs k))
          (lambda (cps/map f xs k)
            ((if (atom xs)
                 (lambda () (k xs))
                 (lambda ()
                   (f (car xs)
                      (lambda (hd)
                        (cps/map cps/map f (cdr xs)
                                 (lambda (tl)
                                   (k (cons hd tl))))))))))))
       ))
    ;; true
    (quote t)
    ;; false
    ()
    ;; cadr
    (lambda (x) (car (cdr x)))
    ;; caddr
    (lambda (x) (car (cdr (cdr x))))
    ;; arity1
    (cons () ())
    ;; arity2
    (cons () (cons () ()))
    ;; arity3
    (cons () (cons () (cons () ())))
    ;; print
    (lambda (tag x) x)
    ;; error
    (lambda (reason) (lambda (x skip) (cons reason x)))
    ))
