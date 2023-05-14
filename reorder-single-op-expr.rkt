#lang racket

;; Script to generate all the possible evaluation orders for
;; an arithmetic expression with a single arithmetic operator.

;; TODO it would be useful to have a macro to translate the
;; expression syntax using the _ to an expression syntax using
;; '() to represent the terminal nodes.

(provide reorder-single-op-expr)


;; Serialise the operations in an expression.
(define (serialise-expr tree)
  (match tree
    [(== '_) '()]
    [(list op (== '_) (== '_)) (list op)]
    [(list op (== '_) b)
     (cons op (serialise-expr b))]
    [(list op a (== '_))
     (append (serialise-expr a) (list op))]
    [(list op a b)
     (append (serialise-expr a)
             (list op)
             (serialise-expr b))]))

#|
(serialise-iso-prec-expr '(+ _ _)) ;; expected '(+)
(serialise-iso-prec-expr '(+ (* _ _) _))  ;; expected '(* +)
(serialise-iso-prec-expr '(* _ (+ _ _)))  ;; expected '(* +)
(serialise-iso-prec-expr '(+ (+ _ _ ) (+ _ (* _ _))))  ;; expected '(+ + + *)
|#

;; Given a tree and a list of operations, returns an expression where
;; the operations are stored in the appropriate spot on the tree, such
;; that applying serialise-expr after deserialise-expr returns the input
;; list for all the binary trees with n nodes.
;; Assumes that the number of nodes is equal to the length of the list.
(define (deserialise-expr tree ops)
  ;; Helper. Returns: (partial tree, remaining operations)
  (define (deserialise-helper tree ops)
    (match tree
      [(== '_) (values '_ '())]
      [(list (== '_) (== '_))
       (values `(,(car ops) _ _) (cdr ops))]
      [(list (== '_) b)
       (let-values
           ([(b-tree ops-remaining) (deserialise-helper b (cdr ops))])
         (values (list (car ops) '_ b-tree)
                 ops-remaining))]
      [(list a (== '_))
       (let-values
           ([(a-tree ops-remaining) (deserialise-helper a ops)])
         (values (list (car ops-remaining) a-tree '_)
                 (cdr ops-remaining)))]
      [(list a b)
       (let*-values ([(a-tree ops-a) (deserialise-helper a ops)]
                     [(op) (car ops-a)]
                     [(b-tree ops-b) (deserialise-helper b (cdr ops-a))])
         (values (list op a-tree b-tree)
                 ops-b))]))
  ;; Main function.
  (let-values ([(expr _) (deserialise-helper tree ops)])
    expr))

#|
(deserialise-expr '(_ _) '(+))  ;; expected '(+ _ _)
(deserialise-expr '(_ (_ _)) '(* +))  ;; expected '(* _ (+ _ _))
(deserialise-expr '((_ _) (_ _)) '(* * +))  ;; expected '(* (* _ _ ) (+ _ _))
|#


;; Returns a list with all the single-operation trees with n nodes.
;; The trees are returned in a form without the operations as these
;; are not known at this stage. For example, instead of returning
;; (+ (+ _ _) (+ _ _)), it just returns: ((_ _) (_ _)).
(define (make-single-op-trees n)
  (cond [(zero? n) '(_)]
        [(eq? n 1) '((_ _))]
        [else
         (for/fold ([res '()])
                   ([m (in-range n)])
           (append
            (let* ([p (- (sub1 n) m)]
                   [trees-m (make-single-op-trees m)]
                   [trees-p (make-single-op-trees p)])
              (for*/list ([ti trees-m] [tj trees-p])
                (list ti tj)))
            res))]))

#|
(length (make-single-op-trees 2)) ;; 2
(length (make-single-op-trees 4)) ;; 14
(length (make-single-op-trees 5)) ;; 42
(length (make-single-op-trees 6)) ;; 132
(length (make-single-op-trees 7)) ;; 429
|#

;; Given an expression, returns all the possible reorderings that are
;; equivalent to the given expression but differ in the evaluation
;; order. It is assumed that the input expression only has operations
;; with the same precedence.
;; Ex. '(+ (+ _ _) _)
;; -> '((+ (+ _ _) _) (+ _ (+ _ _)))
(define (reorder-single-op-expr tree)
  (let* ([ops (serialise-expr tree)]
         [n (length ops)]
         [all-trees (make-single-op-trees n)])
    (for/list ([t all-trees])
      (deserialise-expr t ops))))
