#lang racket

(require "reorder-expr.rkt")
(require rackunit rackunit/text-ui)

;; unordered version of check-equal?
;; This is not really equivalent to a multiset comparison.
(define (check-uequal? a b)
  (and (check-eq? (length a) (length b))
       (check-equal? (list->set a) (list->set b))))


(define-test-suite test-reorder-expr
  (test-case
   "single-op"
   (check-equal? (stream->list (equivalent-exprs '(+ _ _)))
                 '((+ _ _)))
   (check-uequal? (stream->list (equivalent-exprs '(+ _ (+ _ _))))
                  '((+ (+ _ _) _) (+ _ (+ _ _)))))
  (test-case
   "sum-and-mul"
   (check-uequal? (stream->list (equivalent-exprs '(+ _ (* (+ _ _) (* _ _)))))
                  '((+ _ (* (* (+ _ _) _) _)) (+ _ (* (+ _ _) (* _ _)))))))


(run-tests test-reorder-expr)
