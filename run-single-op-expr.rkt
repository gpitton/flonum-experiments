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
      (list op '_ (depth-n-left op (sub1 n)))))


(define depth 10)
(define op '*)

(define ex (depth-n-left op depth))
(define all-ex (reorder-single-op-expr ex))
(displayln (length all-ex))
;; take only the values to strip out the symbols.
(define args (hash-values (loguniform-sample (expt 2 depth))))


(define (evaluate-single-op-expr expr args)
  ;; Helper to keep track of the arguments that still need to be evaluated.
  ;; The expression is the car of expr-args, the arguments are in the cdr.
  (define (evaluate-helper expr-args)
    (match (car expr-args)
      [(== '_) args]
      [(list op lhs rhs)
       (let* ([fl-op (op->flop op)]
              [lhs-v (evaluate-helper (cons lhs args))]
              ;; the arguments yet to be used are stored in (cdr lhs-v)
              [rhs-v (evaluate-helper (cons rhs (cdr lhs-v)))])
         (cons
          ;; value of the current expression
          (fl-op (car lhs-v) (car rhs-v))
          ;; remaining arguments.
          (cdr rhs-v)))]))
  ;; The result is the first element of the list.
  (car (evaluate-helper (cons expr args))))


;; evaluate all the elements of all-e3 on the argument args
(define results
  (for/set ([ex all-ex])
    (evaluate-single-op-expr ex args)))
(displayln results)