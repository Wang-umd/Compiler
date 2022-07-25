#lang racket
(require "ast.rkt")
(provide optimizer optimizer-env)

;;Main optimizer
(define (optimizer p)
  (match p
    [(Prog ds e)
     (Prog ds (optimizer-env e '() ds))]))

;;Main optimizer
(define (optimizer-env e r ds)
  (match e
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (lookup r x)]
    [(Str s)  (Str s)]
    [(Prim0 'void) (Prim0 'void)]
    [(Prim0 'read-byte) (Prim0 'read-byte)]
    [(Prim0 'peek-byte) (Prim0 'peek-byte)]
    [(Prim1 p e) (match (optimizer-env e r ds)
                   ['err 'err]
                   [v (optimizer-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (optimizer-env e1 r ds)
       ['err 'err]
       [v1 (match (optimizer-env e2 r ds)
             ['err 'err]
             [v2 (optimizer-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (optimizer-env e1 r ds)
       ['err 'err]
       [v1 (match (optimizer-env e2 r ds)
             ['err 'err]
             [v2 (match (optimizer-env e3 r ds)
                   ['err 'err]
                   [v3 (optimizer-prim3 p v1 v2 v3)])])])]
    [(If p e1 e2)
     (match (optimizer-env p r ds)
       ['err 'err]
       [v (match v
            [(Bool #f) (optimizer-env e2 r ds)]
            [_ (optimizer-env e1 r ds)])])]
    [(Begin e1 e2)
     (match e1
       [(Prim1 'write-byte a) e]
       [_
     (match (optimizer-env e1 r ds)
       ['err 'err]
       [_    (optimizer-env e2 r ds)])])]
    [(Let x e1 e2)
     (match e2
       [(Begin a b) e]
       [_
     (match (optimizer-env e1 r ds)
       ['err 'err]
       [v (optimizer-env e2 (ext r x v) ds)])])]
    [(App f es)
       (match (optimizer-env* es r ds)
         ['err 'err]
         [vs
          (match f
            [(Var fn)
           (match (defns-lookup ds fn)
            [(Defn f xs e)
             (if (= (length xs) (length vs))
                 (optimizer-env e (zip xs vs) ds)
                 'err)]
             [_ 'err])]
            [_ 'err])])]
    [_ e]
    )
  )

;; Op1 optimizer
(define (optimizer-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (Int i))            (Int (add1 i))]
    [(list 'sub1 (Int i))            (Int (sub1 i))]
    [(list 'zero? (Int i))           (Bool (zero? i))]
    [(list 'char? v)                      (match v
                                            [(Char c) (Bool #t)]
                                            [_ (Bool #f)])]
    [(list 'char->integer (Char c))       (Int (char->integer c))]
    [(list 'integer->char v)              (match v
                                            [(Int i) (if (codepoint? i) (Char (integer->char i)) 'err)]
                                            [_ 'err])]
    [(list 'eof-object? v)                (Prim1 'eof-object? v)]
    [(list 'write-byte v)                 (match v
                                            [(Int i) (if (and (> i 0) (<= i 256)) (Prim1 'write-byte v) 'err)]
                                            [_ 'err])]
    [(list 'box v)                        (Prim1 'box v)]
    [(list 'unbox (Prim1 'box v))         v]
    [(list 'car (Prim2 'cons v1 v2))                v1]
    [(list 'cdr (Prim2 'cons v1 v2))                v2]
    [(list 'empty? v)                     (match v
                                            [(Empty) (Bool #t)]
                                            [_ (Bool #f)])]
    [(list 'cons? v)                      (match v
                                            [(Prim2 'cons v1 v2) (Bool #t)]
                                            [_ (Bool #f)])]
    [(list 'box? v)                       (match v
                                            [(Prim1 'box x) (Bool #t)]
                                            [_ (Bool #f)])]
    [(list 'vector? v)                    (match v
                                            [(Prim2 'make-vector v1 v2) (Bool #t)]
                                            [_ (Bool #f)])]
    [(list 'vector-length (Prim2 'make-vector v1 v2))    v1]
    [(list 'string? v)                    (match v
                                            [(Prim2 'make-string v1 v2) (Bool #t)]
                                            [(Str s) (Bool #t)]
                                            [_ (Bool #f)])]
    [(list 'string-length v)              (match v
                                            [(Prim2 'make-string v1 v2) v1]
                                            [(Str s) (Int (string-length s))]
                                            [_ 'err])]
    [_                                    'err]))

;; Op2 optimizer
(define (optimizer-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (Int i1) (Int i2))  (Int (+ i1 i2))]
    [(list '- (Int i1) (Int i2))  (Int (- i1 i2))]
    [(list '< (Int i1) (Int i2))  (Bool (< i1 i2))]
    [(list '= (Int i1) (Int i2))  (Bool (= i1 i2))]
    [(list 'cons v1 v2)                   (Prim2 'cons v1 v2)]
    [(list 'eq? v1 v2)                    (match v1
                                            [(Int i) (match v2
                                                       [(Int i2) (Bool (eq? i i2))]
                                                       [_ (Bool (eq? v1 v2))])]
                                            [(Bool i) (match v2
                                                       [(Bool i2) (Bool (eq? i i2))]
                                                       [_ (Bool (eq? v1 v2))])]
                                            [(Char i) (match v2
                                                       [(Char i2) (Bool (eq? i i2))]
                                                       [_ (Bool (eq? v1 v2))])]
                                            [(Str i) (match v2
                                                       [(Str i2) (Bool (eq? i i2))]
                                                       [_ (Bool (eq? v1 v2))])]
                                            [_ (Bool (eq? v1 v2))])]
    [(list 'make-vector (Int i) v) (if (<= 0 i) (Prim2 'make-vector (Int i) v) 'err)]
    [(list 'vector-ref (Prim2 'make-vector (Int i1) v) (Int i2))
     (if (<= 0 i2 (sub1 i1)) (vector-ref (make-vector i1 v) i2) 'err)]
    [(list 'make-string (Int i) v) (if (<= 0 i) (Prim2 'make-string (Int i) v) 'err)]
    [(list 'string-ref s (Int i)) (match s
                                    [(Str s1) (if (<= 0 i (sub1 (string-length s1))) (Char (string-ref s1 i)) 'err)]
                                    [(Prim2 'make-string (Int i2) (Char c)) (if (<= 0 i (sub1 i2)) (Char (string-ref (make-string i2 c) i)) 'err)]
                                    [_ 'err])]
    [_ 'err]))

;; Op3 optimizer
(define (optimizer-prim3 p v1 v2 v3)
  (match (list p v1 v2 v3)
    [(list 'vector-set! (Prim2 'make-vector (Int i1) b) (Int i2) c) (if (<= 0 i2 (sub1 i1)) (vector-set! (make-vector i1 b) i2 c) 'err)]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

;;Add variable
(define (ext r x i)
  (cons (list x i) r))

;; Defns Symbol -> [Maybe Defn]
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

;;Lookup variable
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

;;To list
(define (optimizer-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (optimizer-env e r ds)
       ['err 'err]
       [v (match (optimizer-env* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

;;Variable list
(define (zip xs ys)
    (match* (xs ys)
      [('() '()) '()]
      [((cons x xs) (cons y ys))
       (cons (list x y)
             (zip xs ys))]))