
(define-syntax page-response
  (syntax-rules ()
    ((_ item)
     (page-response (220 "ok") item))
    ((_ code item)
     (page-response code "text/html" item))
    ((_ code type (expr ...))
     (page-response "construct" code type (shtml->html (expr ...))))
    ((_ code type content)
     (page-response "construct" code type content))
    ((_ "construct" code type content)
     (let-http-response code
       (let-headers ((content-type type))
         (let-content-length content))))))
