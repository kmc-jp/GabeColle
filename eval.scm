(define (atom? x) (not (pair? x)))
(define (at0 x) (car x))
(define (at1 x) (car (cdr x)))
(define (at2 x) (car (cdr (cdr x))))
(define (at3 x) (car (cdr (cdr (cdr x)))))

(define primitives
  (list (list 'atom (lambda (x) (if (atom? (at0 x)) 't '())))
        (list 'eq   (lambda (x) (if (eq? (at0 x) (at1 x)) 't '())))
        (list 'cons (lambda (x) (cons (at0 x) (at1 x))))
        (list 'car  (lambda (x) (if (atom? (at0 x)) '() (car (at0 x)))))
        (list 'cdr  (lambda (x) (if (atom? (at0 x)) '() (cdr (at0 x)))))))
(define (eval exp env)
  (if (atom? exp) (lookup exp env)
      (let ((fn (eval (car exp) env)))
        (cond
         ((eq? fn 'if) (eval (if (not (eq? '() (eval (at1 exp) env)))
                                 (at2 exp) (at3 exp)) env))
         ((eq? fn 'quote) (at1 exp))
         ((eq? fn 'lambda) (list 'lambda (at1 exp) (at2 exp) env))
         (else
          (let ((args (eval-list (cdr exp) env)))
            (if (atom? fn) ((lookup fn primitives) args)
                (let ((params (at1 fn)) (body (at2 fn)) (local (at3 fn)))
                  (eval body (append (zip params args) local env))))))))))
(define (eval-list x env)
  (if (atom? x) x
      (cons (eval (car x) env)
            (eval-list (cdr x) env))))
(define (lookup key env)
  (if (atom? env) key
      (if (eq? key (at0 (car env))) (at1 (car env))
          (lookup key (cdr env)))))
