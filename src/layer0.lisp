(lambda (exp)
  ((lambda (eval) (eval eval exp))
   (lambda (f exp)
     ((lambda (and2 or2 at0 at1 at2 at3 list2 list3 arity1 arity2 arity3 false)
        ((lambda (foldr foldl fold2)
           ((lambda (result same append)
              ((lambda (subst check update then)
                 ((lambda (eval skip)
                    (skip
                     (skip
                      (foldl skip (result (if (atom exp) (quote ok) (quote skip)) exp)
                             (foldr append ()
                                    (list3 (list3 (lambda (exp)
                                                    (update (check exp (quote quote) arity1)
                                                            (lambda (exp) (at1 exp))))
                                                  (lambda (exp)
                                                    (update (check exp (quote lambda) arity2)
                                                            (lambda (exp) exp)))
                                                  (lambda (exp)
                                                    (then (check exp (quote if) arity3)
                                                          (lambda (exp)
                                                            (then (eval (at1 exp))
                                                                  (lambda (br)
                                                                    (eval (if (eq () br) (at3 exp) (at2 exp)))))))))
                                           (list3 (lambda (exp)
                                                    (then (foldr (lambda (a d)
                                                                   (then (eval a) (lambda (h)
                                                                                    (update d (lambda (t)
                                                                                                (cons h t))))))
                                                                 (result (quote ok) ()) exp)
                                                          (lambda (exp)
                                                            (result (quote skip) exp))))
                                                  (lambda (exp)
                                                    (update (check exp (quote atom) arity1)
                                                            (lambda (exp) (if (atom (at1 exp)) (quote t) ()))))
                                                  (lambda (exp)
                                                    (update (check exp (quote eq) arity2)
                                                            (lambda (exp) (if (eq (at1 exp) (at2 exp)) (quote t) ())))))
                                           (list3 (lambda (exp)
                                                    (update (check exp (quote cons) arity2)
                                                            (lambda (exp) (cons (at1 exp) (at2 exp)))))
                                                  (lambda (exp)
                                                    (update (check exp (quote car) arity1)
                                                            (lambda (exp) (if (atom (at1 exp)) () (car (at1 exp))))))
                                                  (lambda (exp)
                                                    (update (check exp (quote cdr) arity1)
                                                            (lambda (exp) (if (atom (at1 exp)) () (cdr (at1 exp))))))))))
                      (lambda (exp)
                        (then (check (car exp) (quote lambda) arity2)
                              (lambda (fn)
                                (if (same (at1 fn) (cdr exp))
                                    (eval (subst (at2 fn) (at1 fn) (cdr exp) ()))
                                  (result (quote arity) exp))))))
                     (lambda (exp)
                       (result (quote stuck) exp))))
                  (lambda (exp) ;; env
                    (f f exp))
                  (lambda (r f) ;; skip
                    (if (eq (quote skip) (at0 r)) (f (at1 r)) r))
                  ))
               (lambda (exp keys vals params) ;; subst
                 ((lambda (subst) (subst subst exp keys vals params))
                  (lambda (subst exp keys vals params)
                    (if (atom exp) (if (foldr (lambda (p r) (or2 (eq exp p) r)) false params) exp
                                     (fold2 (lambda (k v r) (if (eq k exp) (list2 (quote quote) v) r))
                                            (lambda (k v) exp) keys vals))
                      (if (eq (quote quote) (at0 exp)) exp
                        (if (eq (quote lambda) (at0 exp))
                            (list3 (at0 exp) (at1 exp) (subst subst (at2 exp) keys vals (append (at1 exp) params)))
                          (foldr (lambda (x r) (cons (subst subst x keys vals params) r)) () exp)))))))
               (lambda (exp tag arity) ;; check
                 (result (if (atom exp) (quote stuck) (if (eq tag (at0 exp)) (if (same arity exp) (quote ok) (quote arity)) (quote skip))) exp))
               (lambda (res f) ;; update
                 (if (eq (quote ok) (at0 res)) (result (quote ok) (f (at1 res)))
                   res))
               (lambda (res f) ;; then
                 (if (eq (quote ok) (at0 res)) (f (at1 res)) res))
               ))
            (lambda (st exp) ;; result
              (list2 st exp))
            (lambda (xs ys) ;; same
              (fold2 (lambda (xa ya d) d) (lambda (x y) (and2 (atom x) (atom y))) xs ys))
            (lambda (xs ys) ;; append
              (foldr cons ys xs))
            ))
         (lambda (f z xs) ;; foldr
           ((lambda (foldr) (foldr foldr f z xs))
            (lambda (foldr f z xs)
              (if (atom xs) z
                (f (car xs) (foldr foldr f z (cdr xs)))))))
         (lambda (f z xs) ;; foldl
           ((lambda (foldl) (foldl foldl f z xs))
            (lambda (foldl f z xs)
              (if (atom xs) z
                (foldl foldl f (f z (car xs)) (cdr xs))))))
         (lambda (f z xs ys) ;; fold2
           ((lambda (fold2) (fold2 fold2 f z xs ys))
            (lambda (fold2 f z xs ys)
              (if (or2 (atom xs) (atom ys)) (z xs ys)
                (f (car xs) (car ys) (fold2 fold2 f z (cdr xs) (cdr ys)))))))
         ))
      (lambda (x y) (if x y x)) ;; and2
      (lambda (x y) (if x x y)) ;; or2
      (lambda (x) (car x)) ;; at0
      (lambda (x) (car (cdr x))) ;; at1
      (lambda (x) (car (cdr (cdr x)))) ;; at2
      (lambda (x) (car (cdr (cdr (cdr x))))) ;; at3
      (lambda (a b) (cons a (cons b ()))) ;; list2
      (lambda (a b c) (cons a (cons b (cons c ())))) ;; list3
      (cons () (cons () ())) ;; arity1
      (cons () (cons () (cons () ()))) ;; arity2
      (cons () (cons () (cons () (cons () ())))) ;; arity3
      () ;; false
      ))))
