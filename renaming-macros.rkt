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
;;                 [Environment (U Not-Macro Procedure)]
;;              -> SExp [Environment Symbol]
;; renames all the identifiers so that there is no shadowing.  expands macros
;; as it goes
(define (rename&expand sexp renamed-env value-env)
  (match sexp
    ((? literal?) (values sexp renamed-env))
    ((? primitive-syntax?) (values sexp renamed-env))
    ((? symbol?) (ref-stx (rename-id sexp renamed-env)))
    ((list head tail ...)
     (let [(renamed-id (lookup-in-env head renamed-env))]
       (match (lookup-in-env renamed-id value-env)
         ((? lambda-stx?) (rename&expand-lambda renamed-id
                                                tail
                                                renamed-env
                                                value-env))
         ((? not-macro?)  (rename&expand/many-sexps sexp
                                                    renamed-env
                                                    value-env))
         (macro           (rename&expand-macro macro
                                               sexp
                                               renamed-env
                                               value-env)))))))

;; rename&expand-lambda : Symbol
;;                        [ListOf SExp]
;;                        [Environment Symbol]
;;                        [Environment (U Not-Macro Procedure)]
;;                     -> SExp [Environment Symbol]
;; Renames the lambda's arguments and adds them to the value env. Then,
;; calls rename&expand on the body with the new environments
(define (rename&expand-lambda id-bound-to-lambda tail renamed-env value-env)
  (let ([args (first tail)]
        [body (rest tail)])
    (let-values
        ([(renamed-args new-renamed-env) (rename-ids args renamed-env)])
      (let ([new-value-env (extend-env/frame (map (lambda (arg)
                                                    (list arg (not-macro)))
                                                  renamed-args)
                                             value-env)])
        (printf "before body\n")
        (let-values
            ([(renamed-body renamed-env-from-body)
              (rename&expand/many-sexps body
                                        new-renamed-env
                                        new-value-env)])
          (values `(,id-bound-to-lambda (,@(map ref-stx
                                                renamed-args))
                                        renamed-body)
                  renamed-env-from-body))))))

;; rename-ids : [ListOf Symbol] [Environment Symbol]
;;           -> [ListOf Symbol] [Environment Symbol]
(define (rename-ids list-of-ids renaming-environment)
  (pair->values
   (foldr (lambda (id renamed-ids+env)
            (let ([renamed-ids (first renamed-ids+env)]
                  [env         (second renamed-ids+env)])
              (let-values ([(renamed-id new-env) (find-new-id id env)])
                (list (cons renamed-id renamed-ids)
                      new-env))))
          (list '() renaming-environment)
          list-of-ids)))

;; pair->values : (list X Y) -> X Y
(define (pair->values pair)
  (values (first pair) (second pair)))

(define (rename&expand/many-sexps sexps renamed-env env)
  (pair->values
   (foldr (lambda (sexp result+env)
            (let ([result (first result+env)]
                  [renamed-env (second result+env)])
              (printf "before call to rename&expand ~a : ~a : ~a\n"
                      sexp
                      renamed-env
                      env)
              (let*-values
                  ([(new-sexp new-renamed-env)
                    (rename&expand sexp renamed-env env)])
                (list (cons new-sexp result)
                      new-renamed-env))))
          (list '() renamed-env)
          sexps)))

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
        (values new-id (extend-env/binding id new-id
                                           renamed-env))))))

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
