(define (string-split string . pred+max)
  (let-optionals* pred+max ((pred whitespace?)
                            (max #f))
    (let ((proc (string-or-chars->predicate pred)))
      (with-input-from-string
          string
        (lambda ()
          (let lp ((max (or max -1)))
            (if (or (eof-object? (peek-char)) (= max 0))
                '()
                (let ((section (next-chunk proc)))
                  (consume-chars proc)
                  (cons section
                        (lp (- max 1)))))))))))

(define (whitespace? ch)
  (or (char=? ch #\space)
      (and (char>=? ch #\tab)
           (char<=? ch #\return))))

(define (consume-chars pred . port)
  (let-optionals* port ((port (current-input-port)))
    (let ((current (peek-char port)))
      (or (eof-object? current)
          (and (pred current)
               (begin
                 (read-char)
                 (consume-chars pred port)))))))

(assert
 (string-split "foo   bar" whitespace? 3) => '("foo" "bar")
 (string-split "foo bar" whitespace? 1) => '("foo")
 (string-split "foo   bar") => '("foo" "bar"))

(define-syntax let-list
  (syntax-rules ()
    ((_ (bindings expr) body ...)
     (apply expr (lambda bindings body ...)))))

(define-syntax thunk
  (syntax-rules ()
    ((_ body ...)
     (lambda () body ...))))

(define-syntax with-current-output-port
  (syntax-rules ()
    ((_ port body ...)
     (call-with-current-output-port
      port (lambda () body ...)))))

(define (extend this by)
  (lambda ()
    (this)
    (by)))

;;;; server
(define (server-close-object) server-close-object)
(define (server-close-object? obj) (eq? obj server-close-object))

(define-record-type response rtd/response
  (make-response status head body)
  (status r/status)
  (head r/head)
  (body r/body))

(define (http:start-server ip port handler)
  (let ((handler (handle-handler handler)))
    (let lp ()
      (let ((result
             (call-with-values
                 (socket-accept
                  (open-socket port))
               handler)))
        (or (server-close-object? result)
            (lp))))))

(make-fluid current-response)

(define (handle-handler handler)
  (lambda (port output-port)
    (let-list ((version method path) (string-split (read-line port)))
      (let-list ((status head body)
                 (handler port method path null-response))
        (output-response output-port
                         version
                         status
                         head
                         body)))))

(define (output-response output-port version status header body)
  (let ((body (delay (body))))
    (if (or #t (string-ci=? version "HTTP/1.0")) ; no difference for now
        (let ((head 
               (merge-headers
                (headers
                 (content-length body)
                 '(connection . close))
                (header))))
          (with-current-output-port
           output-port
           (output-status status version)
           (output-head head)
           (output-body body))))))

(define (http:respond status head body)
  (let-list ((status1 head1 body1) (current-response))
    (set-fluid!
     current-response
     (list (or status status1)
           (or (and head (extend head1 head)) head1)
           (or (and body (extend body1 body)) body1)))))

(define (noop) noop)

(define null-response (list stat noop))

(define (output-status status version)
  (disp status " " version)
  (crlf))

(define (output-head head)
  (for-each-pair (lambda (key val)
                   (disp (->string key)
                         ": "
                         (->string val))
                   (crlf))
                 head))

(define (body->byte-vector body)
  (with-byte-output-port
   (output (force body))))

(define merge-headers update-force-alist)

(define headers list)

(define (content-length body)
  (let ((size (size-in-bytes (force body))))
    `(content-length . ,size)))

;;;;
(define (consume-http-input in)
  (call-with-values
      (read-http-version in)
    (lambda (version method path)
      (let ((mime (read-mime-headers in)))
        (make-http-request
         version
         method
         (http-path-parse path)
         (mime-header "host" mime)
         (http-extract-query (mime-header "content-type" mime)))))))


