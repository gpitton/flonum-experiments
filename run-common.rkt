#lang racket

;; Common utilities for the runners.

(provide uniform-sample loguniform-sample op->flop expr-depth evaluate-expr x)

(require racket/flonum)


;; Helper to generate a symbol matching a given number
(define (x n)
  (string->symbol (format "x~a" n)))

;; Generates a hash table which maps a symbol to a random number uniformly
;; sampled in the unit interval.
(define (uniform-sample n)
  (let ([rng (make-pseudo-random-generator)])
    (for/hash ([i (in-range n)])
      (values
       (x i)
       (random rng)))))


;; Generates a hash table which maps a symbol to a random number whose log
;; is uniformly sampled in the interval [-6, 0].
(define (loguniform-sample n)
  (let ([rng (make-pseudo-random-generator)])
    (for/hash ([i (in-range n)])
      (values
       (x i)
       (flexpt 10.0 (- (* 7.0 (random rng)) 6.0))))))


;; Return the maximum depth of an expression tree.
(define (expr-depth expr)
  (match expr
    ['() 0]
    [(? symbol?) 0]
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


;; Helper for lookup into op-map.
(define (op->flop op) (lookup op-map op))
      

(define (evaluate-expr expr args)
  (match expr
    [(? number? n) n]
    [(? symbol? n) (hash-ref args n)]
    [(list op lhs rhs)
     (let* ([fl-op (lookup op-map op)]
            [lhs-v (evaluate-expr lhs args)]
            [rhs-v (evaluate-expr rhs args)])
       ;; value of the current expression
       (fl-op lhs-v rhs-v))]))
