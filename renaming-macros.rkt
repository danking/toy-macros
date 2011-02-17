#lang racket

(struct primitive-syntax ())
(struct lambda-stx primitive-syntax ())
(struct if-stx primitive-syntax ())
(struct ref-stx primitive-syntax ())

(define empty-env '())
(define initial-env
  (extend-env/frame (make-frame `((if ,(if-stx))
                                  (lambda ,(lambda-stx))))
                    empty-env))

;; used to bind non-macro identifiers
(struct not-macro ())

;; [AssocList X X] is [ListOf (list X X)]

;; rename&expand : SExp [AssocList Symbol Symbol]
;;                 [Environment (U Not-Macro SExp)]
;;              -> SExp [AssocList Symbol Symbol]
;; renames all the identifiers so that there is no shadowing.  expands macros
;; as it goes
(define (rename&expand sexp renamed-ids env)
  (match sexp
    ((? literal?) (values sexp rename-ids))
    ((? primitive-syntax? (values sexp rename-ids)))
    ((? symbol?) (rename-id sexp renamed-ids))
    ((list head tail)
     (rename&expand (expand head tail renamed-ids env) renamed-ids env))))

;; expand : SExp SExp [Environment (U Not-Macro SExp)]
;;       -> SExp [AssocList Symmbol Symbol]
(define (expand name tail renamed-ids env)
  (let ((maybe-macro (lookup-in-env name env)))
    (if (not-macro? maybe-macro)
        (list head tail)
        (let* ((*updated-renamed-ids* renamed-ids)
               (update (lambda (new-renamed-ids)
                         (set! *updated-renamed-ids* new-renamed-ids))))
          (values (maybe-macro (cons name tail)
                               (get-in-macro-renamer update))
                  *updated-renamed-ids*)))))

;; get-in-macro-renamer : [AssocList Symbol Symbol] (Symbol Symbol -> #<void>)
;;                     -> Symbol
;; takes the `renamed-ids' and produces a procedure which can be passed to a
;; macro and used to rename desired symbols.  This procedure will call
;; `update' every time a new `renamed-ids' association list is created
(define (get-in-macro-renamer renamed-ids update)
  (lambda (id)
    (let-values (((new-id renamed-ids) (rename-id id renamed-ids)))
      (update renamed-ids)
      new-id)))

;; rename-id : Symbol [ListOf (list Symbol Symbol)]
;;          -> Symbol [AssocList Symbol Symbol]
(define (rename-id id renamed-ids)
  (let ((new-id? (assq id renamed-ids)))
    (or new-id?
        (find-new-id id renamed-ids 0))))

;; find-new-id : Symbol [AssocList Symbol Symbol] Number
;;            -> Symbol [AssocList Symbol Symbol]
;; finds a an id which hasn't been mapped to (renamed to) yet and adds it to
;; the renamed-ids, returning the new id and the new renamed-ids
(define (find-new-id id renamed-ids n)
  (let [(new-id (symbol-num id n))]
    (if (ormap (lambda (mapping)
                 (eq? new-id (second mapping)))
               renamed-ids)
        (find-new-id id renamed-ids (add1 n))
        (values (ref-stx new-id) (cons (list id new-id))))))

(define (symbol-num id n)
  (string->symbol (string-append (symbol->string id)
                                 (number->String n))))

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
