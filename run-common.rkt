#lang racket

;; Common utilities for the runners.

(provide uniform-list loguniform-list expr-depth evaluate-expr)

(require racket/flonum)

;; Generates a list with n random numbers uniformly sampled in the unit interval.
(define (uniform-list n)
  (let ([rng (make-pseudo-random-generator)])
    (for/list ([_ (in-range n)])
      (random rng))))


;; Generates a list with n random numbers whose logs are uniformly sampled in the
;; interval [-6, 0].
(define (loguniform-list n)
  (let ([rng (make-pseudo-random-generator)])
    (for/list ([_ (in-range n)])
      (flexpt 10.0 (- (* 7.0 (random rng)) 6.0)))))


;; Return the maximum depth of an expression tree.
(define (expr-depth expr)
  (match expr
    ['() 0]
    [(== '_) 0]
    [(list _ lhs rhs)
     (let ([depth-lhs (expr-depth lhs)]
           [depth-rhs (expr-depth rhs)])
       (add1 (max depth-lhs depth-rhs)))]))


;; Map converting symbols to floating point operations.
(define op-map (list (cons '+ fl+) (cons '* fl*)))


(define (lookup dict key)
  (cond [(null? dict) '()]
        [(eq? (caar dict) key) (cdar dict)]
        [else (lookup (cdr dict) key)]))
      

(define (evaluate-expr expr args)
  ;; Helper to keep track of the arguments that still need to be evaluated.
  ;; The expression is the car of expr-args, the arguments are in the cdr.
  (define (evaluate-helper expr-args)
    (match (car expr-args)
      [(== '_) args]
      [(list op lhs rhs)
       (let* ([fl-op (lookup op-map op)]
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
