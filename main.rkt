; -*- mode: scheme -*-
#lang racket
(require "eval.rkt")
(provide expand)

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
  (my-eval `(,macro ',sexp) init-value-env))
