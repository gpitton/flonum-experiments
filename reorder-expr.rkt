#lang racket

;; Script to generate all the possible evaluation orders for
;; an arithmetic expression using only the operators + and *.


;; In this script we support only + and *, which at evaluation time get replaced
;; by fl+ and fl*.
(define (supported-op? op)
  (or (eq? op '+) (eq? op '*)))


;; Returns #t if the operators x and y have the same precedence.
;; For now supports only fl+ and fl*. Needs to be defined as a macro because in
;; scheme we cannot overload a function.
(define-syntax precedence-eq?
  (syntax-rules ()
    [(_ x y) (eq? x y)]
    [(_ x y z)
     (and (precedence-eq? x y) (precedence-eq? y z))]))


;; Returns #t if x has higher precedence than y.
(define (precedence>? x y)
  (and (eq? x '*) (eq? y '+)))


;; Given an expression, returns a list of sub-expressions with a single
;; operator. The list is built in such a way that the original expression
;; can be reconstructed by passing it to unflatten-expr.
(define (flatten-expr expr)
  (match expr
    ['_ '_]
    [(list op (== '_) (== '_)) `((,op _ _))]
    [(list op-outer (== '_) (list op-inner c d))
     (let ([sub-flat (flatten-expr (list op-inner c d))])
       (if (precedence-eq? op-outer op-inner)
           ;; same operation: keep the current tree.
           (cons
            ;; current sub-expression
            `(,op-outer _ ,(car sub-flat))
            ;; nested sub-expressions with operators different than op.
            (cdr sub-flat))
           ;; different operation: switch to a new tree. We use the symbol
           ;; $ as a placeholder to signal that this is the position
           ;; where a the next sub-expression needs to have its root.
           (cons
            ;; close the current sub-expression
            `(,op-outer _ $)
            ;; nested sub-expressions with operators different than op.
            sub-flat)))]
    [(list op-outer (list op-inner a b) (== '_))
     (let ([sub-flat (flatten-expr (list op-inner a b))])
       (if (precedence-eq? op-outer op-inner)
           ;; same operation: keep the current tree.
           (cons
            ;; current sub-expressions
            `(,op-outer ,(car sub-flat) _)
            ;; nested sub-expressions with operators different than op.
            (cdr sub-flat))
           ;; different operation: switch to a new tree. We use the symbol
           ;; $ as a placeholder to signal that this is the position
           ;; where a the next sub-expression needs to have its root.
           (cons
            ;; close the current sub-expression
            `(,op-outer $ _)
            ;; nested sub-expressions with operators different than op.
            sub-flat)))]
    [(list op-outer (list op-left a b) (list op-right c d))
     (let ([sub-ab (flatten-expr (list op-left a b))]
           [sub-cd (flatten-expr (list op-right c d))])
       (cond
         [(precedence-eq? op-outer op-left op-right)
          (cons
           ;; current sub-expression
           `(,op-outer ,(car sub-ab) ,(car sub-cd))
           ;; nested sub-expressions with operators different than op.
           (append (cdr sub-ab) (cdr sub-cd)))]
         [(precedence-eq? op-outer op-left)
          (cons
           ;; current sub-expression
           `(,op-outer ,(car sub-ab) $)
           ;; nested sub-expressions with operators different than op.
           (append (cdr sub-ab) sub-cd))]
         [(precedence-eq? op-outer op-right)
          (cons
           ;; current sub-expression
           `(,op-outer $ ,(car sub-cd))
           ;; nested sub-expressions with operators different than op.
           (append sub-ab (cdr sub-cd)))]
         [else
          ;; op-outer != op-left and op-outer != op-right.
          (cons
           ;; current sub-expression
           `(,op-outer $ $)
           ;; nested sub-expressions with operators different than op.
           (append sub-ab sub-cd))]))]))


(flatten-expr '(+ _ (+ _ _)))
(flatten-expr '(+ _ (* (+ _ _) (* _ _))))
(flatten-expr '(* (+ (+ _ _) _) (+ _ _)))


;; Returns a list with all the single-operation trees with n nodes.
;; The operation is specified by op, and the leaves are picked up
;; from the list leaves.
(define (make-single-op-exprs n op leaves)
  ;; TODO assert length leaves = n + 1
  (cond [(zero? n) (list (car leaves))]
        [(eq? n 1) `((,op ,@(take leaves 2)))]
        [else
         (for/fold ([res '()])
                   ([m (in-range n)])
           (append
            (let* ([p (- (sub1 n) m)]
                   [exprs-m (make-single-op-exprs m op (take leaves (add1 m)))]
                   [exprs-p (make-single-op-exprs p op (drop leaves (add1 m)))])
              (for*/list ([ti exprs-m] [tj exprs-p])
                (list op ti tj)))
            res))]))

#|
(make-single-op-exprs 1 '+ '(_ $)) ;; expected '((+ _ $))
(make-single-op-exprs 2 '* '(_ $ _))  ;; expected '((* (* _ $) _) (* _ (* $ _)))
(make-single-op-exprs 3 '+ '($ $ _ $))
;; expected
'((+ (+ (+ $ $) _) $)
  (+ (+ $ (+ $ _)) $)
  (+ (+ $ $) (+ _ $))
  (+ $ (+ (+ $ _) $))
  (+ $ (+ $ (+ _ $))))
|#
