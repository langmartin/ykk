,open primitives big-util architecture tables

(define (track-resources thunk)
  (collect)
  (let ((start-mem (available-memory))
        (start-run (run-time))
        (start-real (real-time)))    
    (thunk)
    (let ((end-run (run-time))
          (end-real (real-time))
          (end-mem (available-memory)))
      (values (- end-run start-run)
              (- end-real start-real)
              (- start-mem end-mem)))))

(define (run-bench test repeat)
  (do ((i 0 (+ i 1)))
      ((= i repeat) 'done)
    (test)))

(define (compare-tests repeat tests)
  (for-each (lambda (pair)
              (call-with-values (lambda () (track-resources (lambda () (run-bench (cadr pair) repeat))))
                (lambda (run-time real-time mem)
                  (display "; ")
                  (p `(,(car pair) run-time: ,(/ run-time 1000.) real-time: ,(/ real-time 1000.) words: ,mem)))))
            tests))

(define (available-memory)
  (memory-status (enum memory-status-option available) #f))

(define (run-time)
  (time (enum time-option run-time) #f))

(define (real-time)
  (time (enum time-option real-time) #f))

;;;; Slot Reference Test
(define (test-ref ref)
  (lambda ()    
    (for-each (lambda (k)
                (ref k))
              *data-keys*)))

;;;; Data
(define *data*
  `((a . 1)
    (b . 2)
    (c . 3)
    (d . 4)
    (e . 5)
    (f . 6)
    (g . 7)
    (h . 8)
    (i . 9)
    (j . 10)
    (k . 11)
    (l . 12)
    (m . 13)
    (n . 14)
    (o . 15)
    (p . 16)
    (q . 17)
    (r . 18)
    (s . 19)
    (t . 20)
    (u . 21)
    (v . 22)
    (w . 23)
    (x . 24)
    (y . 25)
    (z . 26)))

(define *data-keys* (map car *data*))
(define *data-values* (map cdr *data*))
(define *data-vector* (list->vector *data-values*))
(define *keys-table*
  ((lambda (t)
     (let loop ((i 0) (keys *data-keys*))
       (if (null? keys)
           t
           (begin
             (table-set! t (car keys) i)
             (loop (+ i 1) (cdr keys))))))   
   (make-symbol-table)))

;;;; Tree Data
(define *keys-structure*
  `(m (f (c (a #f #f)
            (b #f #f))
         (j (h (g #f #f)
               (i #f #f))
            (l (k #f #f)
               #f)))      
      (t (p (o (n #f #f)
               #f)
            (r (q #f #f)
               (s #f #f)))
         (w (v (u #f #f)
               #f)
            (y (x #f #f)
               (z #f #f))))))

(define (map-tree proc tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (proc tree))
        (else (map (lambda (t)
                     (map-tree proc t))
                   tree))))

(define (vectorize-tree tree value)
  (if (not tree)
      tree
      (make-tree (car tree)
                 (value (car tree))
                 (vectorize-tree (cadr tree) value)
                 (vectorize-tree (caddr tree) value))))

(define *keys-tree*
  (map-tree (lambda (tree)
              (and tree
                   (cons tree (table-ref *keys-table* tree))))            
            *keys-structure*))

(define *data-tree*
  (map-tree (lambda (tree)
              (and tree
                   (cond ((assq tree *data*) => (lambda (tree) tree))                    
                         (else (cons tree #f)))))            
            *keys-structure*))

(define tree-key caar)
(define tree-value cdar)
(define left cadr)
(define right caddr)

(define (tree-ref tree key)
  (cond ((not tree) #f)
        ((eq? key (tree-key tree)) (tree-value tree))
        ((symbol<? key (tree-key tree)) (tree-ref (left tree) key))
        (else (tree-ref (right tree) key))))

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))

(define symbol=? eq?)

;; --------------------
;; Tests

(define data-tree-ref
  (lambda (key)
    (tree-ref *data-tree* key)))

(define (tree-ref-a t)
  (tree-ref t 'a))

(define (tree-ref-m t)
  (tree-ref t 'm))

(define (tree-ref-z t)
  (tree-ref t 'z))

(define tree-vector-ref
  (lambda (key)
    (let ((found (tree-ref *keys-tree* key)))
      (and found (vector-ref *data-vector* found)))))

(define (tree-vector-ref-a v)
  (vector-ref v (tree-ref *keys-tree* 'a)))

(define (tree-vector-ref-m v)
  (vector-ref v (tree-ref *keys-tree* 'm)))

(define (tree-vector-ref-z v)
  (vector-ref v (tree-ref *keys-tree* 'z)))

;;;; Alist
(define data-alist-ref
  (lambda (key)
    (cond ((assq key *data*) => cdr)
          (else #f))))

(define (alist-ref-a data)
  (cdr (assq 'a data)))

(define (alist-ref-m data)
  (cdr (assq 'm data)))

(define (alist-ref-z data)
  (cdr (assq 'z data)))

(define (list-ref-a data)
  (list-ref data 0))

(define (list-ref-m data)
  (list-ref data 12))

(define (list-ref-z data)
  (list-ref data 25))

(define (my-assq key lst)
  (cond ((null? lst) #f)
        ((eq? (caar lst) key) (car lst))
        (else (my-assq key (cdr lst)))))

(define my-data-alist-ref
  (lambda (key)
    (cond ((my-assq key *data*) => cdr)
          (else #f))))

(define (my-alist-ref-a data)
  (cdr (my-assq 'a data)))

(define (my-alist-ref-m data)
  (cdr (my-assq 'm data)))

(define (my-alist-ref-z data)
  (cdr (my-assq 'z data)))

;;;; Vector
(define (vector-ref-a v)
  (vector-ref v 0))

(define (vector-ref-m v)
  (vector-ref v 12))

(define (vector-ref-z v)
  (vector-ref v 25))

(define data-vector-ref
  (lambda (key)
    (let ((found (table-ref *keys-table* key)))
      (and found (vector-ref *data-vector* found)))))

(define (table-ref-a v)
  (vector-ref v (table-ref *keys-table* 'a)))

(define (table-ref-m v)
  (vector-ref v (table-ref *keys-table* 'm)))

(define (table-ref-z v)
  (vector-ref v (table-ref *keys-table* 'z)))

;;;; Values

(define (test-apply-values)
  (apply values *data-values*))

(define (make-values-closure a b c d e f g h i j k l m n o p q r s t u v w x y z)
  (lambda ()
    (values a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(define (values-ref-a a b c d e f g h i j k l m n o p q r s t u v w x y z)
  a)

(define (values-ref-m a b c d e f g h i j k l m n o p q r s t u v w x y z)
  m)

(define (values-ref-z a b c d e f g h i j k l m n o p q r s t u v w x y z)
  z)

(define data-values-closure
  (call-with-values test-apply-values make-values-closure))

(define data-values-ref
  (call-with-values data-values-closure data-case))

(define-syntax caseq
  (syntax-rules (else)
    ((_ value (else result))
     result)
    ((_ value ((symbol) result) etc ...)
     (if (eq? symbol value)
         (begin result)
         (caseq value etc ...)))))

(define (data-case a b c d e f g h i j k l m n o p q r s t u v w x y z)
  (lambda (key)
    (caseq key
      ((a) a)
      ((b) b)
      ((c) c)
      ((d) d)
      ((e) e)
      ((f) f)
      ((g) g)
      ((h) h)
      ((i) i)
      ((j) j)
      ((k) k)
      ((l) l)
      ((m) m)
      ((n) n)
      ((o) o)
      ((p) p)
      ((q) q)
      ((r) r)
      ((s) s)
      ((t) t)
      ((u) u)
      ((v) v)
      ((w) w)
      ((x) x)
      ((y) y)
      ((z) z)
      (else #f))))

;;;; Benchmarks

;; --------------------
;; Overall Baseline
(compare-tests
 100000
 `((baseline ,(lambda () #f))))

;; without ,bench
; (baseline run-time: 0.03 real-time: 0.033 words: 16)

;; with ,bench
; (baseline run-time: 0.01 real-time: 0.013 words: 7)

;; --------------------
;; Returning multiple values

(compare-tests
 100000
 `((apply-values ,test-apply-values)
   (values-closure ,data-values-closure)))

;; without ,bench
; (apply-values   run-time: 0.12 real-time: 0.121 words: 129143)
; (values-closure run-time: 0.07 real-time: 0.069 words:   2720)

;; with ,bench
; (apply-values   run-time: 0.08 real-time: 0.083 words: 129141)
; (values-closure run-time: 0.03 real-time: 0.037 words:      7)

;; conslusion: having values already in a closure is better

;; --------------------
;; Single Slot References

(compare-tests
 10000
 `((baseline ,(test-ref (lambda (key) #f)))
   (alist-ref ,(test-ref data-alist-ref))
   (my-alist ,(test-ref my-data-alist-ref))
   (values-ref ,(test-ref data-values-ref))
   (vector-ref ,(test-ref data-vector-ref))
   (tree-ref ,(test-ref data-tree-ref))))

;; without ,bench
; (baseline   run-time: 0.08 real-time: 0.077 words: 110007)
; (alist-ref  run-time: 0.15 real-time: 0.151 words: 110746)
; (values-ref run-time: 0.56 real-time: 0.561 words: 111813)
; (vector-ref run-time: 0.34 real-time: 0.335 words: 110991)
; (tree-ref   run-time: 2.5  real-time: 2.504 words: 213197)

;; with ,bench
; (baseline   run-time: 0.07 real-time: 0.073 words: 110007)
; (alist-ref  run-time: 0.11 real-time: 0.112 words: 110747)
; (my-alist   run-time: 0.61 real-time: 0.619 words: 112210)
; (values-ref run-time: 0.55 real-time: 0.557 words: 111763)
; (vector-ref run-time: 0.3  real-time: 0.295 words: 110997)
; (tree-ref   run-time: 2.32 real-time: 2.335 words: 212408)

;; conclusion: ASSQ is much faster, none use much memory

;; --------------------
;; Single Slot References (when slot key is known ahead of time)

;; best-case scenario for ASSQ (searching for first entry)
(compare-tests
 1000000
 `((alist-ref ,(lambda () (alist-ref-a *data*)))
   (my-alist ,(lambda () (my-alist-ref-a *data*)))
   (list-ref ,(lambda () (list-ref-a *data-values*)))
   (vector-ref ,(lambda () (vector-ref-a *data-vector*)))
   (values-ref ,(lambda () (call-with-values data-values-closure values-ref-m)))
   (table-ref ,(lambda () (table-ref-a *data-vector*)))
   (tree-ref ,(lambda () (tree-ref-a *data-tree*)))
   (tree/vector ,(lambda () (tree-vector-ref-a *data-vector*)))))

;; without ,bench
; (alist-ref   run-time: 0.65 real-time: 0.649 words:   1483)
; (vector-ref  run-time: 0.53 real-time: 0.526 words:   1692)
; (values-ref  run-time: 0.92 real-time: 0.925 words:   2772)
; (table-ref   run-time: 1.29 real-time: 1.295 words:   3442)
; (tree-ref    run-time: 9.21 real-time: 9.229 words: 579646)
; (tree/vector run-time: 9.37 real-time: 9.372 words: 580594)

;; with ,bench
; (alist-ref   run-time: 0.26 real-time: 0.256 words:    622)
; (my-alist    run-time: 0.42 real-time: 0.419 words:   1338)
; (list-ref    run-time: 0.45 real-time: 0.445 words:   1356)
; (vector-ref  run-time: 0.26 real-time: 0.259 words:    566)
; (values-ref  run-time: 0.51 real-time: 0.518 words:   1800)
; (table-ref   run-time: 0.99 real-time: 0.989 words:   3424)
; (tree-ref    run-time: 8.22 real-time: 8.234 words: 577088)
; (tree/vector run-time: 8.4  real-time: 8.405 words: 578327)

;; average-case scenario for ASSQ (searching for middle entry)
(compare-tests
 1000000
 `((alist-ref ,(lambda () (alist-ref-m *data*)))
   (my-alist ,(lambda () (my-alist-ref-m *data*)))
   (list-ref ,(lambda () (list-ref-m *data-values*)))
   (vector-ref ,(lambda () (vector-ref-m *data-vector*)))
   (values-ref ,(lambda () (call-with-values data-values-closure values-ref-z)))
   (table-ref ,(lambda () (table-ref-m *data-vector*)))
   (tree-ref ,(lambda () (tree-ref-m *data-tree*)))
   (tree/vector ,(lambda () (tree-vector-ref-m *data-vector*)))))

;; without ,bench
; (alist-ref   run-time: 0.74 real-time: 0.737 words: 1484)
; (vector-ref  run-time: 0.53 real-time: 0.532 words: 2115)
; (values-ref  run-time: 0.93 real-time: 0.923 words: 3245)
; (table-ref   run-time: 1.3  real-time: 1.293 words: 3891)
; (tree-ref    run-time: 0.95 real-time: 0.952 words: 2989)
; (tree/vector run-time: 1.05 real-time: 1.039 words: 3400)

;; with ,bench
; (alist-ref   run-time: 0.33 real-time: 0.336 words:  786)
; (my-alist    run-time: 2.07 real-time: 2.076 words: 6312)
; (list-ref    run-time: 2.02 real-time: 2.026 words: 5909)
; (vector-ref  run-time: 0.26 real-time: 0.257 words: 1325)
; (values-ref  run-time: 0.51 real-time: 0.506 words: 1457)
; (table-ref   run-time: 1.0  real-time: 0.989 words: 3082)
; (tree-ref    run-time: 0.65 real-time: 0.658 words: 1834)
; (tree/vector run-time: 0.71 real-time: 0.714 words: 2267)

;; worst-case scenario for ASSQ (searching for last entry)
(compare-tests
 1000000
 `((alist-ref ,(lambda () (alist-ref-z *data*)))
   (my-alist ,(lambda () (my-alist-ref-z *data*)))
   (list-ref ,(lambda () (list-ref-z *data-values*)))
   (vector-ref ,(lambda () (vector-ref-z *data-vector*)))
   (values-ref ,(lambda () (call-with-values data-values-closure values-ref-z)))
   (table-ref ,(lambda () (table-ref-z *data-vector*)))
   (tree-ref ,(lambda () (tree-ref-z *data-tree*)))
   (tree/vector ,(lambda () (tree-vector-ref-z *data-vector*)))))

;; without ,bench
; (alist-ref   run-time: 0.85  real-time: 0.858  words:   1910)
; (vector-ref  run-time: 0.53  real-time: 0.524  words:   2105)
; (values-ref  run-time: 0.92  real-time: 0.925  words:   2770)
; (table-ref   run-time: 1.29  real-time: 1.288  words:   3857)
; (tree-ref    run-time: 13.22 real-time: 13.251 words: 381908)
; (tree/vector run-time: 12.92 real-time: 12.96  words: 381393)

;; with ,bench
; (alist-ref   run-time: 0.41  real-time: 0.416  words:   1043)
; (my-alist    run-time: 3.84  real-time: 3.846  words:   9859)
; (list-ref    run-time: 3.54  real-time: 3.54   words:  19412)
; (vector-ref  run-time: 0.25  real-time: 0.255  words:    615)
; (values-ref  run-time: 0.51  real-time: 0.51   words:   1860)
; (table-ref   run-time: 1.0   real-time: 0.987  words:   3486)
; (tree-ref    run-time: 12.13 real-time: 12.184 words: 378596)
; (tree/vector run-time: 11.64 real-time: 11.647 words: 379121)

;; conclusion: As expected, ALIST-REF runs in linear time.  VECTOR-REF
;; is best.  Translating a key through a lookup table doesn't to very
;; well.  Trees perform surprisingly badly.  ALIST-REF performs much
;; worse when not defined in the VM.  This implies that there's
;; nothing special about alists per-se, but having a native VM
;; procedure makes the most difference.