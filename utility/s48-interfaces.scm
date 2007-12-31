(define-interface srfi-78-interface
  (export (check :syntax)
          (check-ec :syntax)
          check-report
          check-set-mode!
          check-reset!
          check-passed?))

(define-interface extended-ports-interface
  (export byte-source->input-port char-source->input-port
	  byte-sink->output-port char-sink->output-port
	  make-tracking-input-port make-tracking-output-port
	  make-byte-vector-input-port make-string-input-port
	  make-byte-vector-output-port make-string-output-port
	  byte-vector-output-port-output string-output-port-output
	  limit-output
	  current-row current-column fresh-line

	  call-with-string-output-port		; deprecated
	  write-one-line))

(define-interface i/o-internal-interface
  (export input-port-option		;read.scm
	  output-port-option		;write.scm

	  initialize-i/o                ;init.scm
	  initialize-i/o-handlers!      ;init.scm

	  disclose-port

	  make-buffered-input-port
	  make-buffered-output-port make-unbuffered-output-port

	  make-port-handler
	  port-handler-discloser port-handler-close
	  port-handler-byte port-handler-char port-handler-block
	  port-handler-ready? port-handler-force

	  make-buffered-input-port-handler
	  make-buffered-output-port-handler
	  make-unbuffered-output-port-handler
	 
	  ;port-handler-buffer-proc     ; extended-ports
	  default-buffer-size

	  open-input-port?
	  open-input-port-status
	  make-input-port-closed!
	  open-output-port?
	  open-output-port-status
	  make-output-port-closed!

	  eof-object                   ; users will probably want this

	  note-buffer-reuse!
	  check-buffer-timestamp!

	  force-output-if-open
	  periodically-force-output!
	  periodically-flushed-ports
	  output-port-forcers

	  call-with-current-input-port
	  call-with-current-output-port
	  call-with-current-noise-port))

(define-interface i/o-interface
  (export current-input-port current-output-port
	  close-output-port close-input-port

	  read-byte peek-byte write-byte byte-ready?
	  read-char peek-char
	  char-ready?
	  write-char
	  read-block write-block
	  newline
	  write-string			;write.scm
	  with-current-ports
	  current-error-port
	  current-noise-port
	  force-output			;xport.scm
	  port-text-codec set-port-text-codec!
	  output-port-ready?
	  input-port?
	  output-port?
	  silently
	  make-null-output-port))

(define-interface srfi-1-interface
  (export map for-each member assoc	; redefined from R5RS
	  xcons make-list list-tabulate cons* list-copy 
	  proper-list? circular-list? dotted-list? not-pair? null-list? list=
	  circular-list length+
	  iota
	  first second third fourth fifth sixth seventh eighth ninth tenth
	  car+cdr
	  take       drop       
	  take-right drop-right 
	  take!      drop-right!
	  split-at   split-at!
	  last last-pair
	  zip unzip1 unzip2 unzip3 unzip4 unzip5
	  count
	  append! append-reverse append-reverse! concatenate concatenate! 
	  unfold       fold       pair-fold       reduce
	  unfold-right fold-right pair-fold-right reduce-right
	  append-map append-map! map! pair-for-each filter-map map-in-order
	  filter  partition  remove
	  filter! partition! remove! 
	  find find-tail any every list-index
	  take-while drop-while take-while!
	  span break span! break!
	  delete delete!
	  alist-cons alist-copy
	  delete-duplicates delete-duplicates!
	  alist-delete alist-delete!
	  reverse! 
	  lset<= lset= lset-adjoin  
	  lset-union  lset-intersection  lset-difference  lset-xor
	  lset-diff+intersection
	  lset-union! lset-intersection! lset-difference! lset-xor!
	  lset-diff+intersection!))
