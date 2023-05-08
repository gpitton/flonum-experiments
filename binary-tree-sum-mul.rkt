#lang racket

;; Script to generate all the possible evaluation orders for
;; an arithmetic expression using only the operators + and *.

(require racket/flonum)

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
  (and (eq? x fl*) (eq? y fl-)))


;; Expected input: '(+ (* (+ _ _) _) (+ _ _))
;; represents: ((_ + _) * _) + (_ + _)
;; Given an input expression encoded as above, returns a list with all the possible
;; representations equivalent to the input list but different operation orders.
;; For the example above, the expected output would be:
;; (((_ + _) * _) + (_ + _)
;;  (((_ + _) * _) + _) + _
;; )
;; TODO: there's lots of duplicated code in the part following
;;       (let ... (for*/fold ...))
;;       that could be simplified.
(define (build-trees tree)
  (match tree
    ;; Found a placeholder. We need to use the form with == because otherwise
    ;; _ acts as a wildcard matching anything (list and symbol form).
    [(== (list '_)) '(_)]
    [(== '_) '(_)]
    ;; Leaf node.
    [(list op (== '_) (== '_))
     #:when (procedure? op)
     `(,op _ _)]
    ;; Expression with a sub-expression and a terminal node, same precedence
    ;; (left branch).
    ;; Ex: (a + b) + _ -> ((a + b) + _, a + (b + _))
    [(list op-outer (list op-inner a b) (== '_))
     #:when (precedence-eq? op-outer op-inner)
     (let ([recur-a (build-trees a)]
           [recur-b (build-trees b)])
       (for*/fold ([res '()])
                  ([a-elt recur-a]
                   [b-elt recur-b])
         (cons
          `(,op-outer (,op-inner ,a-elt ,b-elt) _)
          (cons
           `(,op-outer ,a-elt (,op-inner ,b-elt _))
           res))))]
    ;; Expression with a sub-expression and a terminal node, different precedence
    ;; (left branch).
    [(list op-outer (list op-inner a b) (== '_))
     `(,op-outer (,op-inner ,a ,b) _)]
    ;; Expression with a sub-expression and a terminal node, same precedence
    ;; (right branch).
    ;; Ex: _ + (a + b) -> (_ + (a + b), (_ + a) + b)
    [(list op-outer (== '_) (list op-inner a b))
     #:when (precedence-eq? op-outer op-inner)
     (let ([recur-a (build-trees a)]
           [recur-b (build-trees b)])
       (for*/fold ([res '()])
                  ([a-elt recur-a]
                   [b-elt recur-b])
         (cons
          `(,op-outer _ (,op-inner ,a-elt ,b-elt))
          (cons
           `(,op-outer (,op-inner _ ,a-elt) ,b-elt) res))))]
    ;; Expression with a sub-expression and a terminal node, different precedence
    ;; (right branch).
    [(list op-outer (== '_) (list op-inner a b))
     `(,op-outer _ (,op-inner ,a ,b))]
    ;; Expression with two sub-expressions, same precedences. We have to generate
    ;; all possible permutations.
    [(list op-outer (list op-lhs a b) (list op-rhs c d))
     #:when (precedence-eq? op-outer op-lhs op-rhs)
     (let ([recur-a (build-trees a)]
           [recur-b (build-trees b)]
           [recur-c (build-trees c)]
           [recur-d (build-trees d)])
       (for*/fold ([res '()])
                  ([a-elt recur-a]
                   [b-elt recur-b]
                   [c-elt recur-c]
                   [d-elt recur-d])
         (cons
          `(,op-outer (,op-lhs ,a-elt ,b-elt) (,op-rhs ,c-elt ,d-elt))
          (cons
           `(,op-rhs (,op-outer (,op-lhs ,a-elt ,b-elt) ,c-elt) ,d-elt)
           (cons
            `(,op-lhs ,a-elt (,op-outer ,b-elt (,op-rhs ,c-elt ,d-elt)))
            res)))))]
    ;; Expression with two sub-expressions, of which two have the same precedence.
    ;; case with: (a + b) + (c * d) -> (id, a + (b + (c * d)))
    ;; or:        (a * b) * (c + d) -> (id, a * (b * (c + d)))
    [(list op-outer (list op-lhs a b) (list op-rhs c d))
     #:when (and (precedence-eq? op-outer op-lhs)
                 (not (precedence-eq? op-rhs op-lhs)))
     (let ([recur-a (build-trees a)]
           [recur-b (build-trees b)]
           [recur-c (build-trees c)]
           [recur-d (build-trees d)])
       (for*/fold ([res '()])
                  ([a-elt recur-a]
                   [b-elt recur-b]
                   [c-elt recur-c]
                   [d-elt recur-d])
         (cons
          `(,op-outer (,op-lhs ,a-elt ,b-elt) (,op-rhs ,c-elt ,d-elt))
          (cons
           `(,op-lhs ,a-elt (,op-outer ,b-elt (,op-rhs ,c-elt ,d-elt)))
           res))))]
    ;; Expression with two sub-expressions, of which two have the same precedence.
    ;; case with: (a * b) + (c + d) -> (id, ((a * b) + c) + d)
    ;; or:        (a + b) * (c * d) -> (id, ((a + b) * c) * d)
    [(list op-outer (list op-lhs a b) (list op-rhs c d))
     #:when (and (precedence-eq? op-outer op-rhs)
                 (not (precedence-eq? op-rhs op-lhs)))
     (let ([recur-a (build-trees a)]
           [recur-b (build-trees b)]
           [recur-c (build-trees c)]
           [recur-d (build-trees d)])
       (for*/fold ([res '()])
                  ([a-elt recur-a]
                   [b-elt recur-b]
                   [c-elt recur-c]
                   [d-elt recur-d])
         (cons
          `(,op-outer (,op-lhs ,a-elt ,b-elt) (,op-rhs ,c-elt ,d-elt))
          (cons
           `(,op-rhs (,op-outer (,op-lhs ,a-elt ,b-elt) ,c-elt) ,d-elt)
           res))))]
    ;; Expression with two sub-expressions, of which two have the same precedence.
    ;; case with: (a + b) * (c + d) -> (id)
    ;; or:        (a * b) + (c * d) -> (id)
    [(list op-outer (list op-lhs a b) (list op-rhs c d))
     #:when (and (precedence-eq? op-lhs op-rhs)
                 (not (precedence-eq? op-outer op-lhs)))
     (let ([recur-a (build-trees a)]
           [recur-b (build-trees b)]
           [recur-c (build-trees c)]
           [recur-d (build-trees d)])
       (for*/fold ([res '()])
                  ([a-elt recur-a]
                   [b-elt recur-b]
                   [c-elt recur-c]
                   [d-elt recur-d])
         (cons
          `(,op-outer (,op-lhs ,a-elt ,b-elt) (,op-rhs ,c-elt ,d-elt))
          res)))]
    ))
         

(define ex0 '(_))
(build-trees ex0)
(define ex1 `(,fl+ _ _))
(displayln (build-trees ex1))
(define ex2-1 `(,fl+ (,fl* _ _) _))
(displayln (build-trees ex2-1))
(define ex2-2 `(,fl+ (,fl+ _ _) _))
(define ex2-3 `(,fl+ _ (,fl* _ _)))
(displayln (build-trees ex2-3))
(define ex2-4 `(,fl+ _ (,fl+ _ _)))
(displayln (build-trees ex2-4))
(define ex3-1 `(,fl+ (,fl+ _ _) (,fl+ _ _)))
(displayln (build-trees ex3-1))
(define ex3-2 `(,fl+ (,fl* _ _) (,fl* _ _)))
(displayln (build-trees ex3-2))