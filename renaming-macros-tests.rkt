#lang racket
(require rackunit
         "renaming-macros.rkt")

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
                                  (extend-env/frame (make-frame '((a b) (b c)))
                                                    empty-env))
                '(((foo bar) (baz qux)) ((a b) (b c))))))

(test-suite
 "renaming"
 (test-case
  "find-new-id"
  (check-equal? (find-new-id 'foo empty-env)
                (values (ref-stx 'g0) (extend-env/binding 'foo 'g0
                                                          empty-env)))
  (check-equal? (find-new-id 'foo empty-env)
                (values (ref-stx 'g1) (extend-env/binding 'foo 'g1
                                                          empty-env)))))
(test-suite
 "expansion"
 (test-case
  "expand"
  (check-equal? (expand (lambda (x rename) (rename (second x)))
                        '(foo bar)
                        empty-env
                        empty-env)
                (values (ref-stx 'g2) (extend-env/binding 'bar 'g2
                                                          empty-env)))))


(test-suite
 "renaming and expansion"
 (test-case
  "rename&expand"
  (check-equal? (rename&expand* 'a)
                (values (ref-stx 'g3) (extend-env/binding 'a 'g3 empty-env)))
  (check-equal? (rename&expand* '1)
                (values '1 empty-env))
  (check-equal? (rename&expand* '(lambda (foo bar baz) (foo bar baz)))
                (values (list (ref-stx 'g4) (ref-stx 'g5) (ref-stx 'g6))
                        (extend-env/binding
                         'foo 'g4
                         (extend-env/binding
                          'bar 'g5
                          (extend-env/binding
                           'baz 'g6
                           empty-env)))))))

(test-suite
 "initial examples -- probably not valid"
 (test-case
  "rename"
  (check-equal? (rename '(lambda (x)
                           (lambda (x)
                             x)))
                '(lambda (x1)
                   (lambda (x2)
                     x2))))
 (test-case
  "expand"
  (check-equal? (expand '(let ((x 5))
                           x))
                '((lambda (x) x) 5))

  (check-equal? (expand '(let ((x 5))
                           (let ((x 4))
                             x)))
                '((lambda (x) ((lambda (x) x) 4)) 5)))
 (test-case
  "rename&expand"
  (check-equal? (rename&expand '(let ((x 5))
                                  (let ((x 4))
                                    x)))
                '((lambda (x1) ((lambda (x2) x2) 4)) 5))))
