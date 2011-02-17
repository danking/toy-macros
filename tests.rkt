#lang racket
(require rackunit
         "main.rkt")

(define macro-tests
  (test-suite
   "macros"
   (test-case
    "sanity"
    (check-equal? (expand '(lambda (x) x) init-macro-env)
                  '(lambda (x) x))
    (check-equal? (expand '(lambda (x y) (x y)) init-macro-env)
                  '(lambda (x y) (x y))))

   (test-case
    "basic"
    (check-equal? (expand '(sqr 2)
                          (extend 'sqr
                                  '(lambda (sexp)
                                     `(* ,(second sexp) ,(second sexp)))
                                  init-macro-env))
                  '(* 2 2)))

   (test-case
    "simple let"
    (check-equal? (expand '(let (x 3) (+ x x))
                          (extend 'let
                                  '(lambda (sexp)
                                     `((lambda (,(first (second sexp)))
                                         ,@(cddr sexp))
                                       ,(second (second sexp))))
                                  init-macro-env))
                  '((lambda (x) (+ x x)) 3)))

   (test-case
    "real let"
    (check-equal? (expand '(let ((x 3) (y 4))
                             (* 2 (+ x y)))
                          (extend 'let
                                  '(lambda (sexp)
                                     `((lambda (,@(map first (second sexp)))
                                         ,@(cddr sexp))
                                       ,@(map second (second sexp))))
                                  init-macro-env))
                  '((lambda (x y) (* 2 (+ x y))) 3 4)))))x

(define (eval sexp)
  (my-eval sexp init-value-env))

(define eval-tests
  (test-suite
   "eval"
   (test-case
    "eval if"
    (check-equal? (eval '(if true 'true 'false))
                  'true)
    (check-equal? (eval '(if false 'true 'false))
                  'false))
   (test-case
    "addition"
    (check-equal? (eval '(+ 1 2 3)) 6))
   (test-case
    "car/first"
    (check-equal? (eval '(car '(1 2 3))) 1)
    (check-equal? (eval '(first '(1 2 3))) 1))
   (test-case
    "cdr/rest"
    (check-equal? (eval '(cdr '(1 2 3))) '(2 3))
    (check-equal? (eval '(rest '(1 2 3))) '(2 3)))
   (test-case
    "lambda"
    (check-equal? (eval '((lambda (x) x) 4)) 4)
    (check-equal? (eval '((lambda (x y z) z) 1 2 3)) 3))))

(define dans-macros (test-suite "root"
                                macro-tests
                                eval-tests))

(require rackunit/text-ui)
(run-tests dans-macros)
