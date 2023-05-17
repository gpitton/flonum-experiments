#lang racket

(require "run-common.rkt")
(require "reorder-single-op-expr.rkt")


;; Generates a balanced expression consisting of the single operation op,
;; having depth n.
(define (depth-n-balanced op n)
  (if (zero? n)
      '_
      (let ([branch (depth-n-balanced op (sub1 n))])
        (list op branch branch))))


;; Generates an expression consisting of the single operation op,
;; having depth n and skewed on the left.
(define (depth-n-left op n)
  (if (zero? n)
      '_
      (list op (depth-n-left op (sub1 n)) '_)))


(define depth 10)
(define op '*)

(define ex (depth-n-left op depth))
(define all-ex (reorder-single-op-expr ex))
(displayln (length all-ex))
(define args (loguniform-list (expt 2 depth)))


(evaluate-expr ex args)
;; evaluate all the elements of all-e3 on the argument args
(define results
  (for/set ([ex all-ex])
    (evaluate-expr ex args)))
(displayln results)