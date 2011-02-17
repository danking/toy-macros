#lang racket
(provide extend init-value-env init-macro-env my-eval macro? get value)

;; environment
(struct value ())

(define (extend x v env)
  (hash-set env x (cons v (hash-ref env x '()))))

(define (extend-list xs vs env)
  (foldl extend
         env
         xs
         vs))

(define (get x env)
  (let ((values (hash-ref env x '())))
    (if (empty? values)
        (error 'get "no mapping for ~a" x)
        (first values))))

(define (macro? x env)
  (let ((val (hash-ref env x #f)))
    (and (cons? val)
         (not (value? (first val))))))

(define init-macro-env (hasheq))
(define env-with-let
  (extend 'let
          '(lambda (stx)
             `((lambda (,@(map first (second stx)))
                 ,@(rest (rest stx)))
               ,@(map second (second stx))))
          init-macro-env))

(struct closure (params body env))
(struct primop (proc))

(define-syntax create-primops-env
  (syntax-rules ()
    ((create-primops-env x ... env)
     (extend-list '(x ...) (list (primop x) ...) env))))

(define empty-value-env (hasheq))
(define init-value-env
  (create-primops-env first rest second third fourth fifth sixth seventh eighth
                      car cdr cadr cdar cddr
                      + - / *
                      not ormap andmap map foldl foldr
                      (extend-list '(true false)
                                   '(#t #f)
                                   empty-value-env)))

;; simple evaluator
(define (my-eval sexp env)
  (match sexp
    ((? symbol?) (lookup sexp env))
    ((? self-eval?) sexp)
    ((list 'quote quoted-value) quoted-value)
    ((list 'quasiquote body) (my-eval-quasiquote body env))
    ((list 'lambda (list params ...) body ...)
     (closure params body env))
    ((list 'if condition true-case false-case)
     (if (my-eval condition env)
         (my-eval true-case env)
         (my-eval false-case env)))
    ((list head tail ...)
     (let* ((procedure (my-eval head env))
            (args (map (lambda (sexp) (my-eval sexp env)) tail)))
       (if (closure? procedure)
           (my-eval-closure procedure args)
           (my-eval-primop procedure args))))))

(define (lookup id env)
  (if (hash-has-key? env id)
      (get id env)
      (error 'lookup "undefined identifier: ~a in ~a" id env)))

(define (my-eval-closure closure args)
  (let ((params (closure-params closure))
        (body (closure-body closure))
        (env (closure-env closure)))
    (let ((new-env (extend-list params args env)))
      (last (map (lambda (sexp) (my-eval sexp new-env)) body)))))

(define (my-eval-primop primop args)
  (apply (primop-proc primop)
         (map (lambda (x)
                ;; take care of hofs
                (cond [(primop? x) (primop-proc x)]
                      [(closure? x) (lambda args (my-eval-closure x args))]
                      [else x]))
              args)))

;; quasiquote
;; -- I realize this should actually be a macro, but this is easier for now
(struct splice (ls))

(define (my-eval-quasiquote body env)
  (match body
    ((list 'unquote-splicing sexp) ;; unquote-splicing cannot be top-level of qq
     (error 'quasiquote "invalid context within quasiquote at: ~a" body))
    (_ (my-eval-quasiquote* body env))))

(define (my-eval-quasiquote* body env)
  (match body
    ((list 'unquote-splicing value)
     (splice (my-eval value env)))
    ((? qq-self-eval?) body)
    ((list 'unquote sexp) (my-eval sexp env))
    ((list qqvals ...)
     (join-quasiquoted-values qqvals env))))

(define (join-quasiquoted-values qqvals env)
  (foldr (lambda (x xs)
           (let ((e (my-eval-quasiquote* x env)))
             (if (splice? e)
                 (append (splice-ls e) xs)
                 (cons e xs))))
         '()
         qqvals))

(define (qq-self-eval? x)
  (or (string? x) (number? x) (symbol? x)))

(define (self-eval? x)
  (or (string? x)
      (number? x)))
