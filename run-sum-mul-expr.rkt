#lang racket

(require "run-common.rkt")
(require "reorder-expr.rkt")
(require racket/flonum)


;; Records an expression that is the inner product of two vectors of size n.
(define (inner-product n)
  (cond [(zero? n) '_]
        [(eq? n 1) '(* _ _)]
        [else
         `(+ ,(inner-product 1) ,(inner-product (sub1 n)))]))


(define ndim 10)

;; TODO inner-product is not the best example: only length-2 subtrees with
;; the same precedence.
(define p-expr (inner-product ndim))
(define d (expr-depth p-expr))

;(define args (loguniform-list (expt 2 d)))
(define args (uniform-list (expt 2 d)))

(define all-p-exprs (equivalent-exprs p-expr))

;; evaluate all the elements of all-p-expr on the argument args
(define results
  (for/set ([ex all-p-exprs])
    (evaluate-expr ex args)))

(displayln (format "expression depth: ~a" d))
(displayln (format "equivalent expressions: ~a" (length all-p-exprs)))
(displayln results)