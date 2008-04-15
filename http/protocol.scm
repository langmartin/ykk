(define (status-code->phrase code)
  (cond ((number? code)
         (assert-table-ref *numeric-status-codes* code))
        ((string? code)
         (status-code->phrase (string->number code 10)))
        (else
         (error 'wrong-type-argument
                "status-code->phrase: expecting number or string"
                code))))

;;;; Utility
(define (list->table t lst)
  (for-each (lambda (item)
              (let ((key value (unlist item)))
                (table-set! t key value)))
            lst)
  t)

(define (assert-table-ref t key)
  (cond ((table-ref t key) => identity)
        (else
         (error 'table-entry-not-found
                t
                key))))

;;;; Lookup Tables
(define *numeric-status-codes*
  (list->table
   (make-integer-table)
   `((100 "Continue")
     (101 "Switching Protocols")
     (200 "OK")
     (201 "Created")
     (202 "Accepted")
     (203 "Non-Authoritative Information")
     (204 "No Content")
     (205 "Reset Content")
     (206 "Partial Content")
     (300 "Multiple Choices")
     (301 "Moved Permanently")
     (302 "Found")
     (303 "See Other")
     (304 "Not Modified")
     (307 "Temporary Request")
     (400 "Bad Request")
     (401 "Unauthorized")
     (402 "Payment Required")
     (403 "Forbidden")
     (404 "Not Found")
     (405 "Method Not Allowed")
     (406 "Not Acceptable")
     (407 "Proxy Authentication Required")
     (408 "Request Time-out")
     (409 "Conflict")
     (410 "Gone")
     (411 "Length Required")
     (412 "Precondition Failed")
     (413 "Request Entity Too Large")
     (414 "Request-URI Too Large")
     (415 "Unsupported Media Type")
     (416 "Requested range not satisfiable")
     (417 "Expectation Failed")
     (500 "Internal Server Error")
     (501 "Not Implemented")
     (502 "Bad Gateway")
     (503 "Service Unavailable")
     (504 "Gateway Time-out")
     (505 "HTTP Version not supported"))))

;;;; Tests
(begin
  (assert (status-code->phrase 200) => "OK")
  (assert (status-code->phrase "300") => (status-code->phrase 300))
  )

