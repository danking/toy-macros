#lang racket
(require rackunit
         "renaming-macros.rkt")

(check-equal? (rename '(lambda (x)
                         (lambda (x)
                           x)))
              '(lambda (x1)
                 (lambda (x2)
                   x2)))

(check-equal? (expand '(let ((x 5))
                         x))
              '((lambda (x) x) 5))

(check-equal? (expand '(let ((x 5))
                         (let ((x 4))
                           x)))
              '((lambda (x) ((lambda (x) x) 4)) 5))

(check-equal? (rename&expand '(let ((x 5))
                                (let ((x 4))
                                  x)))
              '((lambda (x1) ((lambda (x2) x2) 4)) 5))
