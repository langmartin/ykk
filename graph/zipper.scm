(define-record-type zzipper 
  (zipper curr-node k)
  zipper?
  (curr-node z-curr-node)
  (k z-k))

(define (zip-all-the-way-up zipper)
  (if (not (zipper? zipper))
      zipper
      (zip-all-the-way-up ((z-k zipper) (z-curr-node zipper)))))
