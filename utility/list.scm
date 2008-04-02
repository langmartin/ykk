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
