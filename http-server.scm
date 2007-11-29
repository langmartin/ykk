;;;; Utility
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
      port
      (lambda () body ...)))))

(define (extend this by)
  (lambda ()
    (this)
    (by)))

(define (for-each-pair proc lst)
  (for-each (lambda (pair)
              (proc (car pair)
                    (cdr pair)))
            lst))

(define (crlf . port)
  (apply disp
         (append port
                 '(#\return #\newline))))

;;;; Server
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

(define (handle-handler handler)
  (lambda (port output-port)
    (let-list ((version method path) (string-split (read-line port)))
      (call-with-values
          (handler port method path)
        (lambda (status head body)
          (output-response output-port
                           version
                           status
                           head
                           body))))))

(define (output-response output-port version status header body)
  (if (or #t (string-ci=? version "HTTP/1.0")) ; no difference for now
      (let* ((body (body->byte-vector body))
             (head 
              (merge-headers
               (headers
                (content-length body)
                '(connection . close))
               (header))))
        (with-current-output-port
         output-port
         (output version " " status)
         (crlf)
         (output-head head)
         (display body)))))


;; (define (noop) noop)

;; (define null-response (list stat noop))

(define (output-head head)
  (for-each-pair (lambda (key val)
                   (disp key ": " val #\return #\newline))
                 head)
  (crlf))

(assert
 (with-string-ports
  "" (output-head '((host . coptix.com) (content-length . 456)))) =>
  "host: coptix.com\r\ncontent-length: 456\r\n\r\n")

(define (body->byte-vector body)
  (with-byte-output-port
   (output body)))

(define merge-headers update-alist)

(define headers list)

(define (content-length body)
  `(content-length . ,(byte-vector-length body)))

;;;; Proxy
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
