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

(define ndim 4)
(define n-samples 10000)


(define p-expr (permanent ndim))
(define d (expr-depth p-expr))

(define args (loguniform-sample ndim))
;(define args (uniform-sample ndim))

(define all-p-exprs (stream-take (equivalent-exprs p-expr) n-samples))

;; evaluate all the elements of all-p-expr on the argument args
(define results
  (for/set ([ex all-p-exprs])
    (evaluate-expr ex args)))

(displayln (format "expression depth: ~a" d))
(displayln (format "equivalent expressions: ~a" (stream-length all-p-exprs)))
(displayln results)
