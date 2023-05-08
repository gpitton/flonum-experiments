#lang racket

(require "binary-tree-sum-mul.rkt")
(require rackunit rackunit/text-ui)


(define-test-suite sum-mul-reorder
  (test-case
   "placeholder"
   (define ex0-0 '(_))
   (define ex0-1 '_)
   (check-equal?
    (build-trees ex0-0) '(_))
   (check-equal?
    (build-trees ex0-1) '(_)))
  (test-case
   "depth-one"

   (define ex1-0 '(+ _ _))
   (check-equal? (build-trees ex1-0) `(,ex1-0)))

  (test-case
   "depth-two, two operations"
   (define ex2-0 '(+ (* _ _) _))
   (define ex2-1 '(+ (+ _ _) _))
   (define ex2-2 '(+ _ (* _ _)))
   (define ex2-3 '(+ _ (+ _ _)))

   (define ex2-1-rest '(+ _ (+ _ _)))
   (define ex2-3-rest '(+ (+ _ _) _))

   (check-equal? (build-trees ex2-0) `(,ex2-0))
   (check-equal? (build-trees ex2-1) `(,ex2-1 ,ex2-1-rest))
   (check-equal? (build-trees ex2-2) `(,ex2-2))
   (check-equal? (build-trees ex2-3) `(,ex2-3 ,ex2-3-rest)))

  (test-case
   "depth-two, three operations"
   (define ex3-0 '(+ (+ _ _) (+ _ _)))
   (define ex3-1 '(+ (* _ _) (* _ _)))
   (define ex3-2 '(+ (+ _ _) (* _ _)))

   (define ex3-0-rest '((+ (+ (+ _ _) _) _)
                        (+ _ (+ _ (+ _ _)))
                        (+ (+ _ (+ _ _)) _)
                        (+ _ (+ (+ _ _) _))))
   (define ex3-2-rest '(+ _ (+ _ (* _ _))))

   (check-equal? (list->set (build-trees ex3-0))
                 (list->set `(,ex3-0 ,@ex3-0-rest)))
   (check-equal? (build-trees ex3-1) `(,ex3-1))
   (check-equal? (build-trees ex3-2) `(,ex3-2 ,ex3-2-rest)))

  (test-case
   "depth three, three operations"
   (define ex4-0 '(+ _ (+ _ (+ _ (+ _ _)))))

   (check-equal? (length (build-trees ex4-0)) 14)))


(run-tests sum-mul-reorder)