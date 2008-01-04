(let* ((set (lambda elements
               (make-set = < elements)))
       (five (set 1 2 3 4 5))
       (odd (set 1 3 5 7))
       (even (set 2 4)))

  (assert (in-set? five 5))
  (assert (set-ref odd 3) => 3)

  (assert (set<=? even five))
  (assert (not (set<=? odd five)))
  (assert (set=? even even))
  (assert (not (set=? even odd)))

  (assert (set=? five (set 1 2 3 4 5)))
  (assert (set=? (remove five 3 4) (set 1 2 5)))
  (assert (set=? (union five) five))
  (assert (set=? (union five five) five))
  (assert (set=? (union odd even) (adjoin five 7)))
  (assert (set=? (difference five odd) (set 2 4)))
  (assert (set=? (intersection five odd) (set 1 3 5))))
