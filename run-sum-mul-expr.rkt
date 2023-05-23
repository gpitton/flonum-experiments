#lang racket

(require "run-common.rkt")
(require "reorder-expr.rkt")
(require racket/flonum)

(module+ test
  (require rackunit))


;; Records an expression that is the inner product of two vectors of size n.
(define (inner-product n)
  ;; m: number associated to the current symbol.
  ;; n: current depth.
  (define (inner-helper n m)
    (cond [(zero? n) (x 0)]
          [(eq? n 1) `(* ,(x 0) ,(x 1))]
          [else
           `(+ ,(inner-helper 1 m) ,(inner-helper (sub1 n) (add1 m)))]))
  (inner-helper n 0))


;; Returns an expression for the product of all the elements in a list.
(define (make-product lst)
  (cond ;[(symbol? lst) lst]
    [(null? lst) '()]
    [(null? (cdr lst)) (car lst)]
    [else
     `(* ,(car lst)
         ,(make-product (cdr lst)))]))

(module+ test
  (check-equal? (make-product '(x0 x1)) '(* x0 x1))
  (check-equal? (make-product '(x y z)) '(* x (* y z))))


;; Returns an expression for the permanent of an n x n matrix.
(define (permanent n)
  (cond [(zero? n) '()]
        [(eq? n 1) (x 0)]
        [else
         (let ([all-symbols
                (map x (range n))])
           (for/fold ([res '()])
                     ([p (permutations all-symbols)])
             (let ([prod-p (make-product p)])
               (if (null? res) prod-p
                   (list '+ prod-p res)))))]))

(module+ test
  (check-equal? (permanent 2) '(+ (* x1 x0) (* x0 x1)))
  (check-equal? (permanent 3)
                '(+ (* x2 (* x1 x0)) (+ (* x1 (* x2 x0)) (+ (* x2 (* x0 x1))
                 (+ (* x0 (* x2 x1)) (+ (* x1 (* x0 x2)) (* x0 (* x1 x2)))))))))


;; Run a few trials for the permanent expression for a few values of ndim.
(define permanent-trials #hash((3 . 2688) (4 . 10000) (5 . 2000) (6 . 1000) (7 . 200)))
(for ([(ndim n-samples) (in-hash permanent-trials)])
  (displayln (format "dimension: ~a" ndim))
  (let* ([p-expr (permanent ndim)]
         [depth (expr-depth p-expr)])
    (displayln (format "expression depth: ~a" depth))
    (let (
          ;[args (loguniform-sample ndim)]
          [args (uniform-sample ndim)]
          [all-p-exprs (stream-take (equivalent-exprs p-expr) n-samples)])
      (displayln (format "equivalent expressions: ~a" (stream-length all-p-exprs)))
      (let ([results
             ;; evaluate all the elements of all-p-expr on the argument args
             (for/set ([ex all-p-exprs])
               (evaluate-expr ex args))])
        (displayln results)
        (newline)))))
