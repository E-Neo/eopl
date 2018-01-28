#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;; bintree-to-list : BinaryTree -> List
(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) (list 'leaf-node num))
           (interior-node (key left right)
                          (list 'interior-node key
                                (bintree-to-list left)
                                (bintree-to-list right))))))

;; max-interior : BinaryTree -> Sym
;; usage: max-interior takes a binary tree of integers with at least one
;;        interior node and returns the symbol associated with an interior
;;        node with a maximal leaf sum.
