;;;; ,open sxml-tools ,open sxml-tree-trans

(define *tree* (call-with-input-file "/Users/lang/tmp/parsed-rss.scm" read))

(pre-post-order *tree*
                `((*text* . ,(lambda x #f))
                  (*default* . ,(lambda x (cdr x)))
                  (link *preorder* . ,(lambda x x))))

(define sample
  '(begin*
    (*text*
     (lambda x #f))
    (*default*
     (lambda x (cdr x)))
    (link (@ (order preorder))
          (lambda x x))))

(define (unsxml block)
  (values
   (car block)
   (or (sxml:attr-list-node block) '(@))
   (car (sxml:content block))))

(define (assq- key alist)
  (cond ((assq key alist) => cadr)
        (else #f)))

(define-syntax let-unspec*
  (syntax-rules ()
    ((_ alist ((key default) ...) . body)
     (let* ((key (or (assq- 'key alist) default))
            ...)
       .
       body))))

(define (do-lambda env name binding . body)
  (let (new-env (cons-env (zip binding binding) env))
    `(,name
      ,binding
      ,@(map (lambda (expr)
               (evaluate expr new-env))
             body))))

;; (zip '(1 2 3) '(a b c))

(define (lookup-in-env sym env)
  (call-with-current-continuation
   (lambda (found)
     (for-each (lambda (env)
                 (let ((val (sxml:content sym env)))
                   (if val
                       (found val))))
               env)
     sym)))

(define (evaluate lst env)
  (map (lambda (obj)
         (cond ((lambda? obj)
                (apply do-lambda env obj))
               ((symbol? obj)
                (lookup-in-env obj env))
               
               ))))

(define (pre-post-handlers->ppo-arg body)
  (fold (lambda (handler acc)
          (let ((sym attr lambda-lst (unsxml handler)))
            (let-unspec* (cdr attr) ((order #f) (foo 'bar))
              (let* ((lambda-lst (scan-lambda-lst lambda-lst acc))
                     (proc (eval lambda-lst (interaction-environment))))
                (cons (if order
                          (cons* sym (concat->symbol #\* order #\*) proc)
                          (cons* sym proc))
                      acc)))))
       (sxml:content body)))

(pre-post-handlers->ppo-arg sample)
