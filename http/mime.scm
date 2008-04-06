;;;; Compatibility
(define (next-token-of pred port)
  (next-chunk
   (make-not (string-or-chars->predicate pred))
   port))

(define (parser-error port message . text)
  (let-current-output-port
      (current-error-port)
    (apply disp "parser: " port " " message " " text)
    (newline)))

(define (skip-while skip-chars . port)
  (let-optionals* port ((port (current-input-port)))
    (do ((c (peek-char port) (peek-char port)))
        ((not (memv c skip-chars)) c)
      (read-char port))))

(define-syntax w/s
  (syntax-rules ()
    ((_ str body ...)
     (let-string-input-port str body ...))))

(assert
 (w/s "stop here, dude" (skip-while '(#\s #\t))) => #\o)

(define (assert-curr-char expected-chars comment . port)
  (let-optionals* port ((port (current-input-port)))
    (let ((c (read-char port)))
      (if (memv c expected-chars) c
          (parser-error port "Wrong character " c
                        " (0x" (if (eof-object? c) "*eof*"
                                   (number->string (char->integer c) 16)) ") "
                        comment ". " expected-chars " expected")))))

(assert
 (w/s "foo bar" (assert-curr-char '(#\f) "broken")) => #\f)

(define (peek-next-char . port)
  (let-optionals* port ((port (current-input-port)))
    (read-char port)
    (peek-char port)))

(define (read-text-line . port)
  (let-optionals* port ((port (current-input-port)))
    (let ((char-return #\return))
      (if (eof-object? (peek-char port)) (peek-char port)
          (let* ((line
                  (next-token '() (list #\newline #\return (eof-object))
                              "reading a line" port))
                 (c (read-char port))) ; must be either \n or \r or EOF
            (and (eqv? c char-return) (eqv? (peek-char port) #\newline)
                 (read-char port))       ; skip \n that follows \r
            line)))))

;;; from http://okmij.org/ftp/Scheme/lib/input-parse.scm
(define input-parse:init-buffer
  (let ((buffer (make-string 512)))
    (lambda () buffer)))

(define (next-token prefix-skipped-chars break-chars . rest)
  (let-optionals rest ((comment "")
                       (port (current-input-port)))
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

;;;; Oleg's Code
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
	(parser-error port
                      "MIME:parse-content-type: Unexpected empty token "
                      ctype-str)
	res)))
  (call-with-string-input-port ctype-str
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

;; (define (MIME:read-headers port)
;;   (let ((assoc (MIME:read-headers* port)))
;;     (write assoc (current-error-port))
;;     (newline (current-error-port))
;;     assoc))

;;;; Assertions for Oleg's code
(assert
 (call-with-string-input-port
  "Host: header
Content-type: text/html

body"
  MIME:read-headers) =>
  '((content-type . "text/html") (host . "header")))

(assert
 (MIME:parse-content-type "text/html") => '((=mime-type . "text/html"))

 (MIME:parse-content-type "text/html; charset=foo; encoding=bar") =>
 '((encoding . "bar") (charset . "foo") (=mime-type . "text/html")))

;; (call-with-string-input-port
;;  "Server: NS_6.1\r\nlocation: http://apple.accuweather.com/adcbin/apple/Apple_Weather_Data.asp?zipcode=37409\r\n"
;;  MIME:read-headers)

;;;; Data Definitions
(define (e-unimplemented . args)
  (apply error "mime unimplemented" args))

(define (header-assoc tag headers)
  (and-let* ((header (assq tag headers))
             (header (cdr header)))
    header))

(define (header-filter tags headers)
  (filter (lambda (x)
            (not (memq (car x) tags)))
          headers))

(define (read-headers port)
  (let ((headers (MIME:read-headers port)))
    (values
     ;; (header-filter '(content-type) headers)
     headers
     (and-let* ((ct (header-assoc 'content-type headers)))
       (MIME:parse-content-type ct)))))

(define (header-cons key val cdr)
  (cons (cons key val) cdr))

(define header-null '())

(define header-null? null?)

(define (semi-colon-separate . lst)
  (for-each display
            (intersperse "; " lst)))

(define (header->string pair)
  (concat (car pair)
          ": "
          (cdr pair)))

(define (header-split tags headers)
  (values (map (lambda (tag)
                 (or (assq tag headers)
                     (cons tag #f)))
               tags)
          (header-filter tags headers)))

;;;; Interface
(define-record-type mime
  (make-mime head content-type port)
  mime?
  (head mime-headers)
  (content-type mime-content-type)
  (port mime-port set-mime-port!))

(define-record-discloser mime
  (lambda (mime)
    `(mime ,(mime-content-type mime))))

(define (port->mime port)
  (call-with-values
      (lambda () (read-headers port))
    (lambda (headers content-type)
      (make-mime
       headers
       content-type
       port))))

(define (mime->duct mime)
  (let* ((duct (mime->byte-duct mime))
         (ctyp (mime-content-type mime)))
    (if (not ctyp)
        duct
        (charset ctyp (encoding ctyp duct)))))

(define (mime->byte-duct mime)
  (let* ((headers (mime-headers mime))
         (port (mime-port mime))
         (duct (port->duct port)))
    (cond ((xfer-chunked? headers)
           ((d/http-chunked) duct))
          ((content-len headers) =>
           (lambda (len)
             ((d/byte-len len) duct)))
          (else
           ((d/null) duct)))))

(define (mime-stream port)
  (stream-delay
   (if (eof-object? (peek-char port))
       stream-null
       (stream-cons (port->mime port)
                    (mime-stream port)))))

(define port->mime-stream mime-stream)

(define (stream-map->list proc stream)
  (let lp ((stream stream))
    (if (stream-null? stream)
        '()
        (cons (proc (stream-car stream))
              (stream-map->list
               proc
               (stream-cdr stream))))))

(define (mime-read-all port)
  (stream-map->list
   (lambda (mime)
     (cons mime
           (let-string-output-port
            (duct-for-each display (mime->duct mime)))))
   (mime-stream port)))

(define port->mime-list mime-read-all)

(define (xfer-chunked? headers)
  (and-let* ((xf (header-assoc 'transfer-encoding headers))
             (xf (string-downcase xf)))
    (string=? "chunked" xf)))

(define (content-len headers)
  (and-let* ((len (header-assoc 'content-length headers)))
    (string->number len)))

(define (encoding content-type duct)
  (or (and-let* ((enc (header-assoc 'encoding content-type))
                 (enc (string-downcase enc)))
        (case-equal enc
          (("base64")
           ((d/base64) duct))
          (else
           (e-unimplemented "no encoding for" enc))))
      duct))

(define (charset content-type duct)
  (or (and-let* ((set (header-assoc 'charset content-type))
                 (set (string-downcase set)))
        (case-equal set
          (("us-ascii")
           ((d/ascii) duct))
          (else #f)))
      ((d/ascii) duct)))

#;
(define (mimetype content-type duct)
  (or (and-let* ((type (header-assoc '=mime-type content-type))
                 (type (string-downcase set)))
        (case-equal type
          (("application/x-form-urlencoded")
           ((d/urlencode) duct))))
      duct))

(define read-section duct->string)

(define *sample-message*
  "Host: coptix.com\r
Content-type: text/plain; encoding=base64; charset=us-ascii\r
Content-Length: 31\r
\r
aGVsbG8gdGhlcmUsIGZvb2Jhcg==\r
")

(assert
 (cdar
  (call-with-string-input-port *sample-message* mime-read-all))
 => "hello there, foobar")

;; (call-with-string-input-port
;;  "test: me\r\nfoo: bar"
;;  mime-read-all)

(define (mime-content-type-type mime)
  (let ((ctype (mime-content-type mime)))
    (if ctype
        (string->symbol (header-assoc '=mime-type ctype))
        'text/empty)))

(assert
 (mime-content-type-type
  (call-with-string-input-port *sample-message* port->mime)) =>
  'text/plain)
