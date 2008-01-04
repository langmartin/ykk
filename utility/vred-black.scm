(define (record-equal? rec1 rec2)
  (recursive-record-pred? rec1 rec2 equal? #f))

(define (record-value-equal? rec1 rec2)
  (recursive-record-pred? rec1 rec2 equal? #t))

; insert-transform (see Okasaki)
(let* ((solution (red (black 'a 'b 'x)
                      (black 'c 'd 'z)
                      'y))
       (assert (lambda (node)
                 (let ((balanced (insert-transform node (left node) (right node))))                   
                   (if (not (record-equal? solution balanced))
                       (p (list 'failed
                                (node->list node)
                                '=>
                                (node->list balanced))))))))

  (assert
   (black (red 'a (red 'b 'c 'y) 'x) 'd 'z))
  (assert
   (black 'a (red 'b (red 'c 'd 'z) 'y) 'x))
  (assert
   (black 'a (red (red 'b 'c 'y) 'd 'z) 'x))
  (assert
   (black (red (red 'a 'b 'x) 'c 'y) 'd 'z)))

; delete-transform
((lambda ()
  
    (define (node-equal? test try expect)
      (if (not (record-equal? try expect))
          (p (list test 'failed (node->list try) '=> (node->list expect)))))

    (define (assert-transform test try expect)
      (if (not (eq? try expect))
          (p (list test 'transform-failed try '=> expect))))  

    (receive (subtree transform?)
        (delete-transform (black 'x (red (black 'a 'b 'B)
                                         (black 'c 'd 'D)
                                         'C)
                                 'A)
                          left
                          #t)    
      (node-equal? 'left/reduction
                   subtree
                   (black (red 'x (black 'a 'b 'B) 'A)
                          (black 'c 'd 'D)
                          'C)))
   

    (receive (subtree transform?)
        (delete-transform (black (red (black 'a 'b 'B)
                                      (black 'c 'd 'D)
                                      'C)
                                 'x
                                 'A)
                          right
                          #t)    
      (node-equal? 'right/reduction
                   subtree
                   (black (black 'a 'b 'B)
                          (red (black 'c 'd 'D) 'x 'A)                          
                          'C)))

    (receive (subtree transform?)
        (delete-transform (red (black 'a 'b 'node-a)
                               (black 'c 'd 'node-c)
                               'node-b)
                          left)
      (node-equal? 'left/case-1
                   subtree
                   (red (black 'a 'b 'node-a)
                        (red 'c 'd 'node-c)
                        'node-b))
      (assert-transform 'left/case-1 transform? #t))

    (receive (subtree transform?)
        (delete-transform (red (black 'a 'b 'node-a)
                               (black 'c 'd 'node-c)
                               'node-b)
                          right)
      (node-equal? 'right/case-1
                   subtree
                   (red (red 'a 'b 'node-a)
                        (black 'c 'd 'node-c)
                        'node-b))
      (assert-transform 'right/case-1 transform? #t))

    (receive (subtree transform?)
        (delete-transform (red (black 'a 'b 'node-a)
                               (black 'c (red 'd 'e 'node-d) 'node-c)
                               'node-b)
                          left)
      (node-equal? 'left/case-2
                   subtree
                   (red (black (black 'a 'b 'node-a) 'c 'node-b)
                        (black 'd 'e 'node-d)
                        'node-c))
      (assert-transform 'left/case-2 transform? #f))

    (receive (subtree transform?)
        (delete-transform (red (black (red 'a 'b 'node-a) 'c 'node-b)
                               (black 'd 'e 'node-d)
                               'node-c)
                          right)
      (node-equal? 'right/case-2
                   subtree
                   (red (black 'a 'b 'node-a)
                        (black 'c (black 'd 'e 'node-d) 'node-c)
                        'node-b))
      
      (assert-transform 'left/case-2 transform? #f))

    (receive (subtree transform?)
        (delete-transform (red (black 'a 'b 'node-a)
                               (black (red 'c 'd 'node-c) 'e 'node-d)
                               'node-b)
                          left)
      (node-equal? 'left/case-3
                   subtree
                   (red (black (black 'a 'b 'node-a) 'c 'node-b)
                        (black 'd 'e 'node-d)
                        'node-c))
      (assert-transform 'left/case-2 transform? #f))

    (receive (subtree transform?)
        (delete-transform (red (black 'a (red 'b 'c 'node-b) 'node-a)
                               (black 'd 'e 'node-d)
                               'node-c)
                          right)
      (node-equal? 'right/case-3
                   subtree
                   (red (black 'a 'b 'node-a)
                        (black 'c (black 'd 'e 'node-d) 'node-c)
                        'node-b))
      
      (assert-transform 'left/case-2 transform? #f))    
    
    ))


; degenerate cases
(let ((assert (lambda (expect try-tree)
                (if (record-equal? (root try-tree) expect)
                    try-tree
                    (p (list 'failed
                             (node->list (root try-tree))
                             '!=
                             (node->list expect)))))))
  (assert (black (red #f #f 'a)
                 (red #f #f 'z)
                 'foo)
          (insert (assert (black (red #f #f 'a) #f 'foo)
                          (insert (assert (black #f #f 'foo)
                                          (insert (r/b-symbol-tree) 'foo))
                                  'a))
                  'z)))

; test invariants
(letrec ((count-black (lambda (node acc counted)
                        (if (not node)
                            (cons counted acc)
                            (let ((counted (+ counted (if (black? node) 1 0))))                               
                              (count-black (left node)
                                           (count-black (right node)
                                                        acc
                                                        counted)
                                           counted)))))
         (red-have-black-parents (lambda (node)
                                   (if (not node)
                                       #t
                                       (and (red-have-black-parents (left node))
                                            (or (black? node)
                                                (and (not (red? (left node)))
                                                     (not (red? (right node)))))
                                            (red-have-black-parents (right node))))))
         (invariants-hold (lambda (name tree)
                            (if (not (apply = (count-black (root tree) '() 0)))
                                (p (list name 'failed
                                         'equal-black-count
                                         (tree->node-list tree))))
                             (if (not (red-have-black-parents (root tree)))
                                 (p (list name 'failed
                                          'red-have-black-parents
                                          (tree->node-list tree))))
                            (if (not (apply (tree< tree) (tree/in-order->list tree)))
                                (p (list name 'failed
                                         'correct-ordering
                                         (tree->node-list tree)
                                         (tree/in-order->list tree))))))
         (test-set (lambda (name proc tree set)
                     (let loop ((set set))
                       (if (not (null? set))
                           (begin
                             (invariants-hold name (proc tree (car set)))
                             (loop (cdr set))))))))

  (test-set 'insert/random
            insert
            (insert-set (r/b-number-tree) (list 11))
            (list 2 1 7 5 8 14 15))
  (test-set 'insert/sequence
            insert
            (insert-set (r/b-number-tree) (list 1))
            (list 2 3 4 5 6 7 8 9 10))  

  (let ((basis (insert-set (r/b-number-tree)
                        (list 1 2 3 4 5 6 7 8 9 10))))
    (test-set 'delete-1/left->case-3 delete basis (list 7))
    (test-set 'delete-2/right->early-term delete basis (list 8))
    (test-set 'delete-3/right->case-1 delete basis (list 3))        
    (test-set 'delete-all delete basis (list 1 2 3 4 5 6 7 8 9 10))))

(let* ((basis (insert-set (r/b-number-tree)
                          (list 1 2 3 4)))
       (same (maybe-replace basis 2 (lambda x #f)))
       (new (maybe-replace basis 2 (lambda x #t))))
  (eq? basis same)
  (eq? basis new))
