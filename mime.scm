;;;; compatibilty
(define next-token-of next-chunk)

(define (parser-error port message . text)
  (apply error "parser: " port " " message " " text))

(define (skip-while skip-chars . port)
  (let ((port (optional port (current-input-port))))
    (do ((c (peek-char port) (peek-char port)))
        ((not (memv c skip-chars)) c)
      (read-char port))))

(define-syntax w/s
  (syntax-rules ()
    ((_ str body ...)
     (with-input-from-string str (lambda () body ...)))))

(assert
 (w/s "stop here, dude" (skip-while '(#\s #\t))) => #\o)

(define (assert-curr-char expected-chars comment . port)
  (let ((port (optional port (current-input-port))))
    (let ((c (read-char port)))
      (if (memv c expected-chars) c
          (parser-error port "Wrong character " c
                        " (0x" (if (eof-object? c) "*eof*"
                                   (number->string (char->integer c) 16)) ") "
                                   comment ". " expected-chars " expected")))))

(assert
 (w/s "foo bar" (assert-curr-char '(#\f) "broken")) => #\f)

(define (peek-next-char . port)
  (let ((port (optional port (current-input-port))))
    (read-char port) 
    (peek-char port)))

(define (read-text-line . port)
  (let ((port (optional port (current-input-port)))
        (char-return #\return))
    (if (eof-object? (peek-char port)) (peek-char port)
        (let* ((line
                (next-token '() (list #\newline #\return (eof-object))
                            "reading a line" port))
               (c (read-char port)))	; must be either \n or \r or EOF
          (and (eqv? c char-return) (eqv? (peek-char port) #\newline)
               (read-char port))			; skip \n that follows \r
          line))))

;;; from http://okmij.org/ftp/Scheme/lib/input-parse.scm
(define input-parse:init-buffer
  (let ((buffer (make-string 512)))
    (lambda () buffer)))

(define (next-token prefix-skipped-chars break-chars . rest)
  (let-optionals
   rest
   ((comment "") (port (current-input-port)))
   (let outer ((buffer (input-parse:init-buffer)) (filled-buffer-l '())
               (c (skip-while prefix-skipped-chars port)))
     (let ((curr-buf-len (string-length buffer)))
       (let loop ((i 0) (c c))
         (cond
          ((memv c break-chars)
           (if (null? filled-buffer-l) (substring buffer 0 i)
               (string-concatenate-reverse filled-buffer-l buffer i)))
          ((eof-object? c)
           (if (memq (eof-object) break-chars) ; was EOF expected?
               (if (null? filled-buffer-l) (substring buffer 0 i)
                   (string-concatenate-reverse filled-buffer-l buffer i))
               (parser-error port "EOF while reading a token " comment)))
          ((>= i curr-buf-len)
           (outer (make-string curr-buf-len)
                  (cons buffer filled-buffer-l) c))
          (else
           (string-set! buffer i c)
           (read-char port)             ; move to the next char
           (loop (+ 1 i) (peek-char port)))))))))

(assert
 (w/s "test string." (next-token '(#\t #\e) '(#\n) "comment")) => "st stri")

;;;; the Oleg code
;	Handling of MIME Entities and their parts
;
; According to RFC 2045, "Multipurpose Internet Mail Extensions (MIME)
;  Part One, Format of Internet Message Bodies",
;
; "The term 'entity', refers specifically to the MIME-defined header
; fields and contents of either a message or one of the parts in the
; body of a multipart entity.  The specification of such entities is
; the essence of MIME.  Since the contents of an entity are often
; called the 'body', it makes sense to speak about the body of an
; entity.  Any sort of field may be present in the header of an entity,
; but only those fields whose names begin with "content-" actually have
; any MIME-related meaning."
;
; Specifically, the MIME standard (RFC 2045) defines the following
; MIME-related headers (header fields)
;	Content-type
;	Content-Transfer-Encoding
;	Content-ID
;	Content-Description
;
; Generally we leave content interpretation and processing to a
; user-supplied handler. However, if the MIME entity turns out to
; be composite (multipart), this file provides code to disassemble
; it into separate discrete parts, and have them handled, in turn.
; Composite entities are distinguished by their Content-type (media type)
; of multipart/mixed, multipart/alternative, multipart/parallel,
; multipart/digest, or some other multipart type.
; At present, all of them are handled the same way.


; HTTP character types
; Section "2.2 Basic Rules" of the HTTP 1.1 document

(define (http-token-char? x)
  (or (char-alphabetic? x)
      (char-numeric? x)
      (string-index "!#$%&'*+-.^_`|~" x)))


;------------------------------------------------------------------------
;		Parse the Content-type string
;
; Given a Content-Type string:
;	media-type [; attr=value]*
; return the list of associations (attr . value)
; where attr is a symbol and value is a string.
; The media-type is returned as an association with the type
; '=mime-type'
; See Sections 2.2 and 3.6 of rfc2616 (HTTP/1.1) for syntax of the
; Content-Type string

(define (MIME:parse-content-type ctype-str)
  (define (token-pred c)
    (and (char? c) (http-token-char? c) c))
  (define (non-empty-token-of pred port)
    (let ((res (next-token-of pred port)))
      (if (string=? res "")
	(parser-error port "MIME:parse-content-type: Unexpected empty token ")
	res)))
  (call-with-input-string ctype-str
    (lambda (port)
      (let loop ((attrs 
		  (list (cons '=mime-type
			  (non-empty-token-of
			    (lambda (c)
			      (if (eqv? c #\/) c (token-pred c))) port)))))
	(if (eof-object? (skip-while '(#\space #\tab) port))
	  attrs				; return the attributes
	  (begin
	    (assert-curr-char '(#\;) "semi-colon terminator" port)
	    (skip-while '(#\space #\tab) port)
	    (let ((attr-name
		   (string->symbol (non-empty-token-of token-pred port))))
	      (assert-curr-char '(#\=) "attribute-name separator" port)
	      ; loading attr-value, which is (section 2.2 of HTTP1.1):
	      ;   attr-value = token | quoted-string
	      ;   quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
	      ;   qdtext         = <any TEXT except <">>
	      ;   quoted-pair    = "\" CHAR
	      (cond 
	       ((eqv? #\" (peek-char port))	; we're reading a quoted-string
		(read-char port)		; skip the opening quote
		(let qsloop ((old-fragments '()))
		  (let ((fragments
			 (cons
			  (next-token '() '(#\" #\\)
				      "reading quoted-string" port)
			  old-fragments)))
		    (if (char=? #\" (read-char port))
			(loop		; finished reading the quoted-string
			 (cons
			  (cons
			   attr-name
			   (apply string-append (reverse fragments)))
			  attrs))
		      ; we've read a backslash. Read the next char literally
		      (qsloop (cons (string (read-char port)) fragments))
		      ))))
	       (else			; reading token
		(loop
		 (cons
		  (cons attr-name (non-empty-token-of token-pred port))
		   attrs))))))
	      )))))

; read-headers port
; The procedure reads MIME headers from the port.
; The port will be positioned after the empty line that
; separates the headers.
; Later on, make a separate procedure: read-a-header

(define MIME:read-headers
    (let ()
      (define (read-new-header http-port resp-headers)
	(let ((c (peek-char http-port)))
	  (cond
           ((eof-object? c)
            resp-headers)
	   ((eqv? c #\return)		; An empty line, the end of headers
	    (if (eqv? #\newline (peek-next-char http-port))
		(read-char http-port))	; skip the following \n if any
	    resp-headers)
	   ((eqv? c #\newline)	  ; #\return should have been appeared before
	    (read-char http-port) ; but not all servers are compliant
	    resp-headers)
	   ((char-alphabetic? c)  ; beginning of the new header
	    (let* ((header-name
		    (string->symbol
		     (string-downcase
		      (next-token '() (list #\: #\space #\tab (eof-object)) ""
				  http-port))))
		   (delim (skip-while '(#\space #\tab) http-port))
		   (header-value
		    (if (eqv? delim #\:)
			(begin (read-char http-port)
			       (skip-while '(#\space #\tab) http-port)
			       (read-text-line http-port))
			#f)))
	      (if (string? header-value)
		  (check-cont http-port resp-headers
			      header-name header-value)
		  (error "BAD-HEADER: " resp-headers))))
	   (else
	    (error "BAD-HEADER: " resp-headers)))))

      ; check to see if the value of the header continues on the next line
      (define (check-cont http-port resp-headers
			  header-name header-value)
	(let ((c (peek-char http-port)))
	  (cond
	   ((or (eqv? c #\space) (eqv? c #\tab))	; it continues
	    (let ((cont-value (read-text-line http-port)))
	      (check-cont http-port resp-headers
		    header-name (string-append header-value cont-value))))
	   (else
	    (read-new-header http-port
			     (cons (cons header-name header-value)
				   resp-headers))))))
      (lambda (http-port)
	(read-new-header http-port '()))
      ))

;;;; Assertions for the big functions
(assert
 (call-with-input-string
  "Host: header
Content-type: text/html

body"
  (lambda (port) (MIME:read-headers port))) =>
  '((content-type . "text/html") (host . "header")))

(assert
 (MIME:parse-content-type "text/html") => '((=mime-type . "text/html"))

 (MIME:parse-content-type "text/html; charset=foo; encoding=bar") => 
 '((encoding . "bar") (charset . "foo") (=mime-type . "text/html")))

;;;; Iteration, read and decode properly
(define (mime-read port)
  (stream-delay
   (let ((obj (read port)))
     (if (eof-object? obj)
         stream-null
         (stream-cons obj
                      (mime-read port))))))

(define-record-type mime-stream rtd/mime-stream
  (mime-stream-cons car cdr skip)
  mime-stream?
  (car mime-car)
  (cdr mime-cdr)
  (skip mime-skip))

(define (make-get alist)
  (lambda (sym)
    (cond ((assoc sym content-type) => cdr)
          (else #f))))

(define (chunked port)
  (let ((len (string->number (read-crlf-line port))))
    (readlen len port)))

(define (readlen len port)
  (let ((buffer (make-u8vector len)))
    (read-block buffer len port)
    buffer))

(define (next-crlf-line port output-port)
  (next-chunk-display '(#\return) port output-port #t)
  (if (char=? #\newline (peek-char port))
      (display (read-char port) output-port)
      (next-crlf-line port output-port)))

(define (read-crlf-line port)
  (call-with-string-output-port
   (lambda (output-port)
     (next-crlf-line port output-port))))

(define (double-dash port output-port)
  (let ((dashpos (read-char port)))
    (if (and (char=? #\- dashpos)
             (char=? #\- (peek-char port))
             (read-char port))
        #t
        (begin
          (display dashpos output-port)
          #f))))

(define (read-bound port output-port . rest)
  (let-optionals* rest ((boundary #f))
    (let* ((boundlen (if boundary (string-length boundary) 0))
           (boundbuf (make-u8vector boundlen))
           (boundchk (lambda (port output-port)
                       (let ((chunk (read-block boundbuf boundlen port)))
                         (if (equal? boundary chunk)
                             (begin
                               (double-dash port output-port)
                               #t)
                             (cout output-port chunk))))))
      (if boundary
          (let lp ()
            (if (not (and (double-dash port output-port)
                          (boundchk port output-port)))
                (begin
                  (next-crlf-line port output-port)
                  (lp))))
          (output (port-slurp port))))))

(define (call/parsed-content-type content-type receiver)
  (define get
    (make-get content-type))
  (receiver (get 'mime-type)
            (get 'encoding)
            (get 'charset)))

(define (dispatch-cdr headers port)
  (define get
    (make-get headers))
  (let ((content (or (get 'content-type)
                     '((charset . "utf-8"))))
        (chunked (get 'transfer-encoding))
        (length  (get 'content-length)))
    (let ((reader1
           (call/parsed-content-type content content-reader))
          (len (find-length chunked length)))
      (make-mime-stream
       headers
       (make-read-charset
        charset
        (if chunked
            (make-read-chunked port)
            (if length
                (make-read-length length port)
                (make-read-bound boundary port))))
       
       (skipper len)))))

(call-with-string-output-port
 (lambda (out)
   (set-port-text-codec! out utf-8-codec)
   (for-each-byte-vector
    (lambda (x)
      (display x out))
    (make-byte-vector 10 33))))

(define (for-each-byte-vector proc vec)
  (let ((end (byte-vector-length vec)))
    (let lp ((i 0))
      (if (not (= i end))
          (begin
            (proc (byte-vector-ref vec i))
            (lp (+ i 1)))))))

(define *sample-message*
  "Host: coptix.com\r
Content-type: text/plain;\r
  encoding=base64; charset=utf-8\r
Content-Length: 648\r
\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\r
")

#;
(call-with-input-string
 *sample-message*
 (lambda (in)
   (MIME:read-headers in)
   (call-with-output-file
    "foo" (lambda (p) (display (slurp in) p)))))

(call-with-input-string *sample-message* mime:read)
