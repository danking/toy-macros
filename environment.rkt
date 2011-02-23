#lang racket

(provide make-frame extend-env/frame extend-env/binding
         bound-in-env? lookup-in-env)

;; An [Environment V] is a
;;   [ListOf [Frames V]]
;; A [Frame V] is a
;;   [ListOf [Bindings V]]
;; A [Binding V] is a
;;   (list Symbol V)
;; So, in short, an [Environment V] is a [ListOf [ListOf (list Symbol V)]]

;; make-frame : [ListOf (list Symbol V)] -> [Frame V]
(define (make-frame bindings)
  bindings)

;; extend-env/frame : [Frame V] [Environment V] -> [Environment V]
(define (extend-env/frame frame env)
  (cons frame env))

;; extend-env/binding : Symbol V [Environment V] -> [Environment V]
(define (extend-env/binding id value env)
  (extend-env/frame (make-frame `((,id ,value))) env))

(define (bound-in-env? id env)
  (let loop ((env env))
    (if (empty? env)
        #f
        (let [(value (assq id (first env)))]
          (if value
              #t
              (loop (rest env)))))))

(define (lookup-in-env id env)
  (let loop ((env env))
    (if (empty? env)
        (error 'lookup "unbound identifier: ~a" id)
        (let [(value (assq id (first env)))]
          (if value
              (second value)
              (loop (rest env)))))))
