#lang racket

(require "reorder-single-op-expr.rkt")
(require rackunit rackunit/text-ui)


(define-test-suite sum-reorder
  (test-case
   "placeholder"
   (define ex0 '_)
   (check-equal?
    (reorder-single-op-expr ex0) '(_)))
  (test-case
   "depth-one"

   (define ex1-0 '(+ _ _))
   (check-equal? (reorder-single-op-expr ex1-0) `(,ex1-0)))

  (test-case
   "depth-two, two operations"
   (define ex2-0 '(+ (+ _ _) _))
   (define ex2-1 '(+ _ (+ _ _)))

   (define ex2-0-rest '(+ _ (+ _ _)))
   (define ex2-1-rest '(+ (+ _ _) _))

   (check-equal? (reorder-single-op-expr ex2-0) `(,ex2-0 ,ex2-0-rest))
   (check-equal? (reorder-single-op-expr ex2-1) `(,ex2-1-rest ,ex2-1)))

  (test-case
   "depth-two, three operations"
   (define ex3-0 '(+ (+ _ _) (+ _ _)))
   (define ex3-1 '(+ _ (+ _ (+ _ _))))
   (define ex3-2 '(+ (+ _ (+ _ _)) _))

   (define ex3-res '((+ (+ (+ _ _) _) _)
                     (+ (+ _ (+ _ _)) _)
                     (+ (+ _ _) (+ _ _))
                     (+ _ (+ (+ _ _) _))
                     (+ _ (+ _ (+ _ _)))))

   (check-equal? (reorder-single-op-expr ex3-0) ex3-res)
   (check-equal? (reorder-single-op-expr ex3-1) ex3-res)
   (check-equal? (reorder-single-op-expr ex3-2) ex3-res))

  (test-case
   "depth three, three operations"
   (define ex4-0 '(+ _ (+ _ (+ _ (+ _ _)))))

   (check-equal? (length (reorder-single-op-expr ex4-0)) 14)))


(run-tests sum-reorder)