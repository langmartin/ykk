(define (passthrough port)
  (let ((input (mime-read-all port)))
    (values status
            (cons-header (content-type->header
                          (mime-content-type input))
                         (filter-headers '(content-length transfer-encoding)
                                         (mime-headers (car input))))
            (mime-body input))))

(define (proxy-handler port method path)
  (let ((page (http-get method path port)))
    (call-with-http-reply
     (lambda (version code text)
       (let ((headers (MIME:read-headers page)))
         (values
          (code text)
          headers
          (passthrough page)))))))
