;;;; ,open sxml-tools ,open sxml-tree-trans

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

(define *tree* (call-with-input-file "/Users/lang/tmp/parsed-rss.scm" read))

(define sample
  '(begin*
    (*text*
     (lambda x #f))
    (*default*
     (lambda x x))
    (link (@ (order preorder))
          (lambda x x))
    (begin link)
    (guid (@ (order preorder))
          link)
    (bar (@ (order preorder))
         (lambda (tag . rest)
           (cons (concat->symbol 's: tag)
                 rest)))
    (foo (lambda x `(other-tag ...)))))

(document->let-body sample)
(document->let-parameters sample)
(p
 (document->pre-post-handlers sample))

(p (pre-post
    `(begin
       (foo (@ (bar "value"))))
    sample))

(define (opened->let-parameters opened)
  (map (lambda (binding)
         (list (name (car binding))
               (val (cdr (last-pair binding)))))
       opened))

(begin (@@ (*NAMESPACES* (s ,sample)))
  (mumble ...)
  (div (@@ (*NAMESPACES* (s2 ,sample2)))
    stuff-evaluated-in-open-s2/s ...)
  (foo (@ (bar "value"))))

`(begin
   (s:foo (@ (s:bar "value"))))
;; FOO is applied
`(begin
   (other-tag ...))
