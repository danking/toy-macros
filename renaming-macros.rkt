#lang racket

(provide (all-defined-out))

(struct primitive-syntax () #:transparent)
(struct lambda-stx primitive-syntax () #:transparent)
(struct if-stx primitive-syntax () #:transparent)
(struct ref-stx primitive-syntax (id) #:transparent)

;; used to bind non-macro identifiers
(struct not-macro ())


;; SExp -> SExp [Environment Symbol]
(define (rename&expand* sexp)
  (rename&expand sexp empty-env initial-env))


;; rename&expand : SExp
;;                 [Environment Symbol]
;;                 [Environment (U Not-Macro SExp)]
;;              -> SExp [Environment Symbol]
;; renames all the identifiers so that there is no shadowing.  expands macros
;; as it goes
(define (rename&expand sexp renamed-env env)
  (match sexp
    ((? literal?) (values sexp renamed-env))
    ((? primitive-syntax?) (values sexp renamed-env))
    ((? symbol?) (rename-id sexp renamed-env))
    ((list 'lambda (list args ...) body ...)
     (if (lambda-stx? (lookup-in-env 'lambda env))
         '...
         '...))
    ((list head tail ...)
     (rename&expand-list head tail renamed-env env))))

(define (rename&expand/many-sexps sexps renamed-env env)
  (foldr (lambda (sexp result+ids)
           (let*-values
               (((result renamed-env) result+ids)
                ((new-sexp new-ids) (rename&expand sexp renamed-env env)))
             (values (cons new-sexp result)
                     (cons new-ids renamed-env))))
         (values '() renamed-env)
         sexps))

(define (rename&expand-list head tail renamed-env env)
  (let ((maybe-macro (lookup-in-env head env))
        (sexp (cons head tail)))
    (if (not-macro? maybe-macro)
        (rename&expand/many-sexps sexp
                                  renamed-env
                                  env)
        (rename&expand-macro maybe-macro
                             sexp
                             renamed-env
                             env))))

(define (rename&expand-macro macro sexp renamed-env env)
  (let-values
      (((expanded-sexp new-renamed-env) (expand macro sexp renamed-env env)))
    (rename&expand expanded-sexp new-renamed-env env)))

;; expand : SExp SExp [Environment Symbol] [Environment (U Not-Macro SExp)]
;;       -> SExp [Environment Symbol]
(define (expand macro sexp renamed-env env)
  (let* ((*updated-renamed-env* renamed-env)
         (update (lambda (new-renamed-env)
                   (set! *updated-renamed-env* new-renamed-env))))
    (values (apply-macro macro sexp (get-in-macro-renamer renamed-env update))
            *updated-renamed-env*)))

(define (apply-macro macro sexp renamer)
  (macro sexp renamer))

;; get-in-macro-renamer : [Environment Symbol] (Symbol Symbol -> #<void>)
;;                     -> Symbol
;; takes the `renamed-env' and produces a procedure which can be passed to a
;; macro and used to rename desired symbols.  This procedure will call
;; `update' every time a new `renamed-env' association list is created
(define (get-in-macro-renamer renamed-env update)
  (lambda (id)
    (let-values (((new-id renamed-env) (rename-id id renamed-env)))
      (update renamed-env)
      new-id)))

;; rename-id : Symbol [ListOf (list Symbol Symbol)]
;;          -> Symbol [Environment Symbol Symbol]
(define (rename-id id renamed-env)
  (if (bound-in-env? id renamed-env)
      (values (lookup-in-env id renamed-env) renamed-env)
      (find-new-id id renamed-env)))

;; find-new-id : Symbol [Environment Symbol Symbol]
;;            -> Symbol [Environment Symbol Symbol]
;; finds a an id which hasn't been mapped to (renamed to) yet and adds it to
;; the renamed-env, returning the new id and the new renamed-env
(define find-new-id
  (let ((*gensym-count* 0))
    (lambda (id renamed-env)
      (let [(new-id (symbol-num 'g *gensym-count*))]
        (set! *gensym-count* (add1 *gensym-count*))
        (values (ref-stx new-id) (extend-env/binding id new-id renamed-env))))))

(define (symbol-num id n)
  (string->symbol (string-append (symbol->string id)
                                 (number->string n))))

(define (literal? x)
  (or (number? x) (string? x)))

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

(define empty-env '())
(define initial-env
  (extend-env/frame (make-frame `((if ,(if-stx))
                                  (lambda ,(lambda-stx))))
                    empty-env))

