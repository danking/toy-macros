; -*- mode: scheme -*-
#lang racket
(provide extend extend-list init-env
         expand my-eval)

(define (expand sexp env)
  (match sexp
    ((list 'lambda (list args ...) body ...)
     (list* 'lambda args (map (let ((new-env
                                     (foldl (lambda (id old-env)
                                              (extend id (value) old-env))
                                            env
                                            args)))
                                (lambda (subexp) (expand subexp new-env)))
                              body)))
    ((list head args ...)
     (if (and (symbol? head) (macro? head env))
         (apply-macro (get head env) sexp)
         (map (lambda (subexp) (expand subexp env)) sexp)))
    (_ sexp)))

(define (apply-macro macro sexp)
  (my-eval `(,macro ',sexp)))

;; environment
(define init-env (hasheq))
(define env-with-let
  (extend 'let
          '(lambda (stx)
             `((lambda (,@(map first (second stx)))
                 ,@(rest (rest stx)))
               ,@(map second (second stx))))
          init-env))

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
  (not (empty? (hash-ref env x '()))))

;; simple evaluator
(struct closure (params body env))

(define (my-eval sexp env)
  (match sexp
    ((? symbol?) (lookup sexp env))
    ((? self-eval?) sexp)
    ((list 'quasiquote body) (my-eval-quasiquote body env))
    ((list 'lambda (list params ...) body ...)
     (closure params body env))
    ((list head tail ...)
     (let* ((procedure (my-eval head env))
            (closure (if (symbol? procedure)
                         (get procedure env)
                         procedure))
            (args (map (lambda (sexp) (my-eval sexp env)) tail)))
       (my-eval-closure closure args)))))

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

;; quasiquote
;; -- I realize this should actually be a macro, but this is easier for now
(struct splice (ls))

(define (my-eval-quasiquote body env)
  (match body
    ((list 'unquote-splicing sexp)
     (error 'quasiquote "invalid context within quasiquote at: ~a" body))
    (_ (my-eval-quasiquote))))

(define (my-eval-quasiquote* body env)
  (match body
    ((list 'unquote-splicing value)
     (splice (my-eval value env)))
    ((? qq-self-eval?) body)
    ((list 'unquote sexp) (my-eval sexp env))
    ((list qqvals ...)
     (join-quasiquoted-values qqvals))))

(define (join-quasiquoted-values qqvals)
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
      (number? x)
      (and (cons? x) (eq? 'quote (first x)))))
