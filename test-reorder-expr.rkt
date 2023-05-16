#lang racket

(require "reorder-expr.rkt")
(require rackunit rackunit/text-ui)


(define-test-suite test-reorder-expr
  (test-case
   "single-op"
   (check-equal? (equivalent-exprs '(+ _ _)) '((+ _ _)))
   (check-equal? (equivalent-exprs '(+ _ (+ _ _)))
                 '((+ (+ _ _) _) (+ _ (+ _ _)))))
  (test-case
   "sum-and-mul"
   (check-equal? (equivalent-exprs '(+ _ (* (+ _ _) (* _ _))))
                 '((+ _ (* (* (+ _ _) _) _)) (+ _ (* (+ _ _) (* _ _)))))))


(run-tests test-reorder-expr)
