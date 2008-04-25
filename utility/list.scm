(define (intersperse obj lst)
  (cons (car lst)
        (fold-right (lambda (x acc)
                      (cons obj
                            (cons x
                                  acc)))
                    '()
                    (cdr lst))))

(assert (intersperse #\a '(1 2 3)) => '(1 #\a 2 #\a 3))

(define (fold-append kons nil lst . lsts)
  (if (null? lst)
      (if (null? lsts)
          nil
          (apply fold-append kons nil (car lsts) (cdr lsts)))
      (apply fold-append kons (kons (car lst) nil) (cdr lst) lsts)))

(assert (fold-append cons '() '(1 2) '(3 4) '(5 6)) => '(6 5 4 3 2 1))

(define (fold-right-append kons nil lst . lsts)
  (if (null? lst)
      (if (null? lsts)
          nil
          (apply fold-right-append kons nil (car lsts) (cdr lsts)))
      (kons (car lst) 
            (apply fold-right-append kons nil (cdr lst) lsts))))

(assert (fold-right-append cons '() '(1 2) '(3 4) '(5 6)) => '(1 2 3 4 5 6))

(define (map/cons* proc cons lst)
  (if (null? lst)
      lst
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head))
              (tail1 (map/cons* proc cons tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (map* proc lst)
  (map/cons* proc cons lst))

;; update part of a structure
(define (share foo un-foo proc new-foo)  
  (let* (((values . orig) (un-foo foo))
         ((values . new) (apply proc orig)))    
    (if (every eq? orig new)
        foo
        (apply new-foo new))))

(define (fold-numbers proc nil start stop step)
  (if (= start stop)
      nil
      (fold-numbers
       proc
       (proc start nil)
       (+ start step)
       stop
       step)))

(define (fold-right-numbers proc nil start stop step)
  (if (= start stop)
      nil
      (proc start
            (fold-right-numbers
             proc
             nil
             (+ start step)
             stop
             step))))

(define (fmap-car proc pair . rest)
  (cons (apply proc (car pair) rest) (cdr pair)))

(define (fmap-cdr proc pair . rest)
  (cons (car pair) (apply proc (cdr pair) rest)))

(define (fmap-pair proc pair . rest)
  (apply proc (car pair) (cdr pair) rest))

(define (fmap-cadr proc lst . rest)
  (cons (car lst)
        (cons (apply proc (cadr lst) rest)
              (cddr lst))))

(define (fmap-list proc spec . rest)
  (apply proc (car spec) (cadr spec) rest))

(define (unique-conser eq)  
  (lambda (a lst)
    (if (srfi-1:member a lst eq)
        lst
        (cons a lst))))

;;;; Tests
(begin
  (let* ((item (list 'a 'b))
         (mod1 (share item uncons values cons))
         (mod2 (share item uncons (lambda (a b) (values (symbol->string a) b)) cons)))

    ;; sharing is preserved
    (assert (eq? item mod1))
    (assert (neq? item mod2))
    (assert mod2 => '("a" b))
    (assert (eq? (cdr item) (cdr mod2))))

  (assert (fmap-car + '(1 2) 10) => '(11 2))
  
  (assert (fmap-cdr + `(1 . 2) 10) => `(1 . 12))
  
  (assert (fmap-cadr + '(1 2) 10) => '(1 12))
  
  (assert (map-in-order (cut fmap-pair xcons <>) '((a . 1) (b . 2)))
          => '((1 . a) (2 . b)))
  
  (assert (map-in-order (cut fmap-list cons <>) '((a 1) (b 2)))
          => '((a . 1) (b . 2)))
  
  (assert (fold-right (unique-conser =) '() '(1 2 1 2))
          => (delete-duplicates '(1 2 1 2) =))  
  )