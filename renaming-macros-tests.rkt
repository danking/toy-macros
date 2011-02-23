#lang racket
(require rackunit
         "renaming-macros.rkt")

(provide (all-defined-out))

(define all-tests
  (test-suite
   "root"
   env-tests renaming-tests expansion-tests expansion-and-renaming-tests))

(define env-tests
  (test-suite
   "environment"
   (test-case
    "extend-env/binding"
    (check-equal? (extend-env/binding 'foo 'bar empty-env)
                  '(((foo bar))))
    (check-equal? (extend-env/binding 'foo 'bar
                                      (extend-env/binding 'baz 'qux empty-env))
                  '(((foo bar)) ((baz qux)))))
   (test-case
    "extend-env/frame"
    (check-equal? (extend-env/frame (make-frame '((foo bar) (baz qux)))
                                    (extend-env/frame (make-frame '((a b)
                                                                    (b c)))
                                                      empty-env))
                  '(((foo bar) (baz qux)) ((a b) (b c)))))
   (test-case
    "already-mapped-to?"
    (check-equal? (already-mapped-to? 1 empty-env) #f)
    (check-equal? (already-mapped-to? 1 (extend-env/binding 'x 1 empty-env))
                  #t)
    (check-equal? (already-mapped-to? 1 (extend-env/binding 'x 0 empty-env))
                  #f)
    (check-equal? (already-mapped-to? 2 (extend-env/binding
                                         'y 3
                                         (extend-env/binding
                                          'x 2
                                          empty-env)))
                  #t))))
(define-syntax check-equal-2values?
  (syntax-rules ()
    ((_ test actual)
     (check-equal? (let-values ([(one two) test])
                     (list one two))
                   (let-values ([(one two) actual])
                     (list one two))))))

(define renaming-tests
  (test-suite
   "renaming"
   (test-case
    "find-new-id"
    (check-equal-2values? (find-new-id 'foo empty-env)
                          (values 'foo0
                                  (extend-env/binding 'foo 'foo0
                                                      empty-env))))))
(define expansion-tests
  (test-suite
   "expansion"
   (test-case
    "expand"
    (check-equal-2values? (expand (lambda (x rename) (rename (second x)))
                          '(foo bar)
                          empty-env
                          empty-env)
                  (values (ref-stx 'bar0)
                          (extend-env/binding 'bar 'bar0
                                              empty-env))))))


(define expansion-and-renaming-tests
  (test-suite
   "renaming and expansion"
   (test-case
    "rename&expand-lambda"
    (check-equal-2values? (rename&expand-lambda 'lambda
                                        '((a b) (a b))
                                        initial-rename-env
                                        initial-value-env)
                  (values `(lambda (,(ref-stx 'a0) ,(ref-stx 'b0))
                             (,(ref-stx 'a0) ,(ref-stx 'b0)))
                          (extend-env/binding
                           'a 'a0
                           (extend-env/binding
                            'b 'b0
                            initial-rename-env)))))
   (test-case
    "rename&expand"
    (check-equal-2values? (rename&expand* 'a)
                  (values (ref-stx 'a0)
                          (extend-env/binding 'a 'a0 initial-rename-env)))
    (check-equal-2values? (rename&expand* '1)
                  (values '1 initial-rename-env))
    (check-equal-2values? (rename&expand* '(lambda (foo bar baz) (foo bar baz)))
                  (values (list 'lambda
                                (list (ref-stx 'foo0)
                                      (ref-stx 'bar0)
                                      (ref-stx 'baz0))
                                (list (ref-stx 'foo0)
                                      (ref-stx 'bar0)
                                      (ref-stx 'baz0)))
                          (extend-env/binding
                           'foo 'foo0
                           (extend-env/binding
                            'bar 'bar0
                            (extend-env/binding
                             'baz 'baz0
                             initial-rename-env)))))
    (check-equal-2values? (rename&expand '(let ((x 1) (y 2))
                                    (+ x y))
                                 rename-env/let
                                 value-env/let)
                  (values `((lambda (,(ref-stx 'x0) ,(ref-stx 'y0))
                              (,(ref-stx '+) ,(ref-stx 'x0) ,(ref-stx 'y0)))
                            1 2)
                          (extend-env/binding
                           'x 'x0
                           (extend-env/binding
                            'y 'y0
                            rename-env/let)))))))
