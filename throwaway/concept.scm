;; ,open srfi-1
;; ,open big-util
;; ,open nondeterminism

(define (identity x) x)

(define (intersperse-too-much lst el)
  (fold-right (lambda (x tail)
                (cons el (cons x tail)))
              '()
              lst))

(define (intersperse lst el)
  (cdr (intersperse-too-much lst el)))

(define (map* f lst)
  (if (null? lst)
      '()
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (f head))
              (tail1 (map* f tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (depth-first handle tree)
  (cond ((null? tree) tree)
        ((handle tree) =>
         (lambda (new-tree) new-tree))
        ((not (pair? tree)) tree)
        (else
         (let ((mapped (map* (lambda (kid)
                               (depth-first handle kid))
                             tree)))
           (if (eq? mapped tree)
               tree
               mapped)))))

;; (define (eval* tree env)
;;   (if (atom? tree)
;;       tree
;;       (let ((head (car tree))
;;             (tail (cdr tree)))
;;         (cond ((eq? head 'let)
;;                (eval*
;;                 (cdr tail)
;;                 (env-push env (car tail))))
;;               ((self-eval? head)
;;                head)
;;               ((symbol? head)
;;                (cond ((env-look head env) =>
;;                       identity)
;;                      (else (error "undefined"))))
;;               ((pair? head)
;;                (eval* head env))))))

;; (define (self-eval? x)
;;   (or (string? x)
;;       (number? x)
;;       (char? x)))

;; (define (env-push env bindings)
;;   (cons (eval* (make-alist bindings) env)
;;         env))

;; (define (make-alist lst)
;;   (cons 'list
;;         (map (lambda (pair)
;;                (cons 'cons pair))
;;              lst)))

;; (define (env-look sym env)
;;   (call-with-current-continuation
;;    (lambda (found)
;;      (for-each (lambda (x)
;;                  (cond ((assq x sym) =>
;;                         (lambda (pair)
;;                           (found (cdr pair))))))
;;                env)
;;      #f)))

;; (define (evaluate tree)
;;   (eval (eval* tree '())
;;         (scheme-report-environment 5)))

(depth-first (lambda (tree)
               (if )))

(define *tree* (with-input-from-file "data.scm" read))

(lookup '(1 2 3) '(1 2))

(lookup *tree* '(define news-articles))

(define (path . path)
  (eval
   (lookup *tree*
           (cons 'letrec
                 (intersperse-too-much
                  path 'define)))
   (scheme-report-environment 5)))

(define (post data . path)
  (set!
   *tree*
   (depth-first (lambda (tree)
                  (if (eq? tree (lookup tree path))
                      data
                      #f))
                *tree*)))
