;;;; ,open sxml-tools ,open sxml-tree-trans

(define *tree* (call-with-input-file "/Users/lang/tmp/parsed-rss.scm" read))

(define sample
  '(begin*
    (*text*
     (lambda x #f))
    (*default*
     (lambda x (cdr x)))
    (link (@ (order preorder))
          (lambda x x))
    (guid (@ (order preorder))
          link)))

;; (pre-post-order *tree*
;;                 `((*text* . ,(lambda x #f))
;;                   (*default* . ,(lambda x (cdr x)))
;;                   (link *preorder* . ,(lambda x x))))

(define (unsxml block)
  (values
   (car block)
   (or (sxml:attr-list-node block) '(@))
   (car (sxml:content-u block))))

(define (sxml:content-u form)
  (cond ((null? (cdr form))
         '())
        ((and (pair? (cadr form))
              (eq? '@ (caadr form)))
         (cddr form))
        (else
         (cdr form))))

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

;; (define (do-lambda env name binding . body)
;;   (let (new-env (cons-env (zip binding binding) env))
;;     `(,name
;;       ,binding
;;       ,@(map (lambda (expr)
;;                (evaluate expr new-env))
;;              body))))

;; ;; (zip '(1 2 3) '(a b c))

;; (define (lookup-in-env sym env)
;;   (call-with-current-continuation
;;    (lambda (found)
;;      (for-each (lambda (env)
;;                  (let ((val (sxml:content sym env)))
;;                    (if val
;;                        (found val))))
;;                env)
;;      sym)))

;; (define (evaluate lst env)
;;   (map (lambda (obj)
;;          (cond ((lambda? obj)
;;                 (apply do-lambda env obj))
;;                ((symbol? obj)
;;                 (lookup-in-env obj env))
               
;;                ))))

;; (define-syntax define-runtime-syntax
;;   (syntax-rules ()
;;     ((_ name rules)
;;      (define name
;;        (let ((transform (car rules)))
;;          (lambda form
;;            (let* ((eval-form (cons 'name form))
;;                   (result (transform eval-form identity eq?)))
;;              (if (eq? result eval-form)
;;                  (error 'runtime-syntax-error
;;                         'name
;;                         form)
;;                  result))))))))

;; (define-runtime-syntax example
;;   (syntax-rules ()    
;;     ((_ a b)
;;      (list a b))
;;     ((_ a)
;;      (example "1-ary" a))))

(define (document->let-body body)
  (list 'quasiquote
   (fold-right
    (lambda (handler acc)
      (let ((sym attr lambda-lst (unsxml handler)))
        (let-unspec* (cdr attr) ((order #f))
          (let* ((proc (list 'unquote sym)))            
            (cons (if order
                      (cons* sym (concat->symbol #\* order #\*) proc)
                      (cons* sym proc))
                  acc)))))
    '()
    (sxml:content body))))

(define (document->let-parameters body)
  (fold-right
   (lambda (handler acc)
     (let ((sym attr lambda-lst (unsxml handler)))
       (cons (list sym lambda-lst)
             acc)))
   '()
   (sxml:content body)))

(define (document->pre-post-handlers body)
  (eval
   `(let* (,@(document->let-parameters body))
      ,(document->let-body body))
   (interaction-environment)))

(define (pre-post form document)
  (pre-post-order
   form
   (document->pre-post-handlers document)))

;; (let* ((foo (lambda)) ...)
;;   (list (cons* 'foo foo))
;;   ...)

(document->let-body sample)
(document->let-parameters sample)
(p
 (document->pre-post-handlers sample))

(p (pre-post *tree* sample))
