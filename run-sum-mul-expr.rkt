#lang racket

(require "run-common.rkt")
(require "reorder-expr.rkt")
(require racket/flonum)


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

(make-product '(x0 x1)) ;'(* x0 x1)
(make-product '(x y z)) ;'(* x (* y z))


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

(permanent 2) ;'(+ (* x1 x0) (* x0 x1))
(permanent 3) ;'(+ (* x2 (* x1 x0)) (+ (* x1 (* x2 x0)) (+ (* x2 (* x0 x1))
              ; (+ (* x0 (* x2 x1)) (+ (* x1 (* x0 x2)) (* x0 (* x1 x2)))))))

(define ndim 4)

;; TODO inner-product is not the best example: only length-2 subtrees with
;; the same precedence.
(define p-expr (permanent ndim))
(define d (expr-depth p-expr))

;(define args (loguniform-list (expt 2 d)))
(define args (uniform-sample (expt 2 d)))

(define all-p-exprs (equivalent-exprs p-expr))

;; evaluate all the elements of all-p-expr on the argument args
(define results
  (for/set ([ex all-p-exprs])
    (evaluate-expr ex args)))

(displayln (format "expression depth: ~a" d))
(displayln (format "equivalent expressions: ~a" (length all-p-exprs)))
(displayln results)