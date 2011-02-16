#lang racket
(require rackunit
         "main.rkt")

(check-equal? (expand '(lambda (x) (+ x 3 4)) init-env)
              '(lambda (x) (+ x 3 4)))
(check-equal? (expand '(sqr 2)
                      (extend 'sqr
                              '(lambda (sexp)
                                 `(* ,(second sexp) ,(second sexp)))
                              init-env))
              '(* 2 2))
(check-equal? (expand '(let (x 3) (+ x x))
                      (extend 'let
                              '(lambda (sexp)
                                 `((lambda (,(first (second sexp)))
                                     ,@(cddr sexp))
                                   ,(second (second sexp))))
                              init-env))
              '((lambda (x) (+ x x) 3)))
(check-equal? (expand '(let ((x 3) (y 4))
                         (* 2 (+ x y)))
                      (extend 'let
                              '(lambda (sexp)
                                 `((lambda (,@(map first (second sexp)))
                                     ,@(cddr sexp))
                                   ,@(map second (second sexp))))
                              init-env))
              '((lambda (x y) (* 2 (+ x y))) 3 4))
