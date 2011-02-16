#lang racket
(require rackunit
         "main.rkt")

;; sanity
(check-eq? (expand '(lambda (x) x) init-env) '(lambda (x) x))
(check-eq? (expand '(lambda (x y) (x y))) init-env '(lambda (x y) (x y)))

;; basic
(check-eq? (expand '(sqr 2)
                      (extend 'sqr
                              '(lambda (sexp)
                                 `(* ,(second sexp) ,(second sexp)))
                              init-env))
              '(* 2 2))

;; simple let
(check-eq? (expand '(let (x 3) (+ x x))
                      (extend 'let
                              '(lambda (sexp)
                                 `((lambda (,(first (second sexp)))
                                     ,@(cddr sexp))
                                   ,(second (second sexp))))
                              init-env))
              '((lambda (x) (+ x x) 3)))

;; real let
(check-eq? (expand '(let ((x 3) (y 4))
                         (* 2 (+ x y)))
                      (extend 'let
                              '(lambda (sexp)
                                 `((lambda (,@(map first (second sexp)))
                                     ,@(cddr sexp))
                                   ,@(map second (second sexp))))
                              init-env))
              '((lambda (x y) (* 2 (+ x y))) 3 4))
