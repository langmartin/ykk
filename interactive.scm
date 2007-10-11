,config ,load packages.scm
,open octothorpe-extensions
,open define-record-types signals threads extended-ports
,open srfi-1 srfi-13
,open unit-testing
,open util uuid

;; (define-functional-test
;;   (begin
;;     (define (foo-h . expr)
;;       (cons 'foo expr))
;;     (define-reader-ctor
;;       'foo
;;       foo-h)
;;     (define-reader-ctor
;;       'list
;;       list)
;;     (read
;;      (make-string-input-port
;;       "(foo bar (baz (bif)) #,(list (+ 1 2) #,(foo bit) baz) blit)")))
;;   '(foo bar (baz (bif)) ((+ 1 2) (foo bit) baz) blit))
