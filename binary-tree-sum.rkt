#lang racket
(require racket/flonum)


;; make-node creates a node of a binary tree.
;; op, left, or right can be '(). left or right can be a number.
;; A single leaf is represented with: (make-tree '() num '())
; TODO replace this with a lambda: (λ (args) (op left right))
;(define (make-node op left right) (list op left right))


;; build-trees generates a list with all the binary trees whose nodes have
;; the given operations.
;; The trees are represented as functions accepting a list of operations
;; and returning a function that evaluates each tree.
(define (build-trees ops)
  ;; n operations means n + 1 operands.
  (let ([n (length ops)])
    (cond
      ;; if n = 0, return a list with a single operation that accepts a list
      ;; of arguments and returns the first element of the list of arguments.
      [(eq? n 0) (stream-cons (λ (args) (car args)) empty-stream)]
      ;; This is the only possible binary tree with one node: '(op left right).
      [(eq? n 1) (let ([op (car ops)])
                   (stream-cons (λ (args) (op (car args) (cadr args)))
                                empty-stream))]
      [else
       (stream-fold
        (λ (acc m)
          (stream-append
           (let ([ts-left (build-trees (take (cdr ops) m))]
                 [ts-right (build-trees (drop (cdr ops) m))])
             (for*/stream ([t-left (in-stream ts-left)]
                           [t-right (in-stream ts-right)])
               (λ (args)
                 ;; m operations means m + 1 arguments.
                 (let ([args-left (take args (add1 m))]
                       [args-right (drop args (add1 m))])
                   ((car ops) (t-left args-left)
                              (t-right args-right))))))
           acc))
        empty-stream
        (in-range 0 n))])))


;; Usage:
(define ts1 (build-trees (list fl+)))
(define args1 '(3.0 -1.0))
(define vals1 (stream-map (λ (t) (t args1)) ts1))
(displayln (stream->list vals1))

(define ts2 (build-trees (list fl+ fl+)))
(define args2 '(3.0 -1.0 -0.3))
(define vals2 (stream-map (λ (t) (t args2)) ts2))
(displayln (stream->list vals2))

(define ts3 (build-trees (list fl+ fl+ fl+)))
(define args3 '(3.0 -0.3 -1.24 1.e-8))
(define vals3 (stream-map (λ (t) (t args3)) ts3))
(displayln (stream->list vals3))

(define n 5)
(define ts (build-trees (build-list n (λ (_) fl+))))
;; TODO replace args definition with a random number generator.
(define args (build-list (add1 n) (λ (i) (flcos (->fl i)))))
(define vals (stream-map (λ (t) (t args)) ts))
(define vals-l (stream->list vals))
(displayln vals-l)
;; TODO check that this agrees with the theoretical value.
(displayln (format "count: ~a" (length vals-l)))
(displayln (list->set vals-l))