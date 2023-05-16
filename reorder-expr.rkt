#lang racket

;; Script to generate all the possible evaluation orders for
;; an arithmetic expression using only the operators + and *.

(provide equivalent-exprs)


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


;; Helper for passing the arguments to cartesian-product as a list
;; of lists instead of each list separately.
(define (cartesian-prod xs)
  (apply cartesian-product xs))


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

#|
(flatten-expr '(+ _ _))
(flatten-expr '(+ _ (+ _ _)))
(flatten-expr '(+ _ (* (+ _ _) (* _ _))))
(flatten-expr '(* (+ (+ _ _) _) (+ _ _)))
(flatten-expr '(+ _ (* (+ _ _) (* _ _))))
(flatten-expr '(* (+ (+ _ _) (* (+ (+ _ _) _) (+ _ _))) (+ _ _)))
|#

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
;(make-single-op-exprs 1 '+ '(_ _)) ; expected: '((+ _ _))
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


;; Serialise-leaves collects all the leaves of an expression in the
;; order they are encountered in a depth-first search.
(define (serialise-leaves expr)
  (match expr
    [(? symbol? s) (list s)]
    [(list op a b) (append (serialise-leaves a)
                           (serialise-leaves b))]))


;(serialise-leaves '(+ _ _))


;; Glue-sub-exprs accepts a list of expressions, each of which is required
;; to have a unique operator, and glues them together into a single expression
;; using the placeholder '$ to mark the insertion points between sub-expressions.
(define (glue-sub-exprs exprs)
  ;; The helper returns a list that has one layer of nesting in excess, so the
  ;; main driver will call the helper and return the car.
  (define (glue-helper exprs)
    (if (null? exprs)
        '()
        ;; take the first expression in the list and find the leaves marked with $.
        (let ([current-expr (car exprs)]
              [remaining-exprs (cdr exprs)])
          (match current-expr
            [(== '_) `(_ ,@remaining-exprs)]
            ;; Replace the terminal sign $ with the next sub-expression.
            ['$
             (let ([rem-expr (glue-helper remaining-exprs)])
               rem-expr)]
            [(list op a b)
             ;; The first element in the argument to glue-sub-exprs is the
             ;; current expression, which in this context must be a. Then
             ;; the remaining sub-expressions that need to be used are
             ;; stored in (cdr a-exprs).
             (let* ([a-exprs (glue-helper (cons a remaining-exprs))]
                    [b-exprs (glue-helper (cons b (cdr a-exprs)))])
               ;(displayln (cons a remaining-exprs))
               ;(displayln (cons b (cdr a-exprs)))
               ;; Return the result of glueing together the sub-expressions
               ;; of a with those of b. Then whatever is left is in (cdr b-exprs).
               (cons
                (list op (car a-exprs) (car b-exprs))
                (cdr b-exprs)))]))))
  ;; Main driver.
  (car (glue-helper exprs)))

;(glue-sub-exprs '((+ _ _))) ;expected: '(+ _ _)
;(glue-sub-exprs '((+ _ $) (* _ _)))  ; expected: '(+ _ (* _ _))
;(glue-sub-exprs '((+ _ $) (* (* $ _) _) (+ _ _))) ; expected: '(+ _ (* (* (+ _ _) _) _))

(define (equivalent-exprs expr)
  (let* ([sub-exprs (flatten-expr expr)]
         [all-sub-exprs
          (for/list ([sub-expr sub-exprs])
            (let* ([leaves (serialise-leaves sub-expr)]
                   ;; The number of operations is one less than the number of leaves.
                   [n (sub1 (length leaves))])
              (make-single-op-exprs n (car sub-expr) leaves)))])
    (for/list ([sub-exprs-i (cartesian-prod all-sub-exprs)])
      (glue-sub-exprs sub-exprs-i))))
