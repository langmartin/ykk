(define *log-path* #f)

(define (make-path . paths)
  (concat-for-each
   display
   (intersperse #\/ paths)))

(define (cur path)
  (make-path path "current"))

(define (new path)
  (make-path path "new"))

(define (clean path)
  (make-path path (concat (time-seconds (current-time)) ".s")))

(define init-lock (make-lock))

(define (initialize-logging path)
  (if (log-port)
      (error "the log is initialized")
      (if (not (accessible? path (access-mode write)))
          (error "the logging path is not writable" path)
          (begin
            (obtain-lock init-lock)
            (set! *log-path* path)
            (let ((cur (cur path)))
              (if (accessible? cur (access-mode read))
                  (begin
                    (replay-log-file cur)
                    (rotate-log-and-store-heap))))
            (log-set! (open-output-file (cur path)))
            (release-lock init-lock)))))

(define (replay-log-file path)
  (call-with-input-file
      path
    replay-log-port))

(define (string->byte-vector str)
  (os-string->byte-vector
   (string->os-string str)))

(define (write-ykk-image path)
  (let* ((cur (make-path path "image"))
         (new (concat cur ".new")))
    (write-image (string->byte-vector new)
                 (usual-resumer ykk-image-resumer)
                 (string->byte-vector "YKK Image"))
    (rename new cur)))

(define (ykk-image-resumer command-line)
  0)

(define rotate-lock (make-lock))

(define (rotate-log-and-store-heap)
  (let ((path *log-path*))
    (let ((port (log-port))
          (tmp (new path))
          (cur (cur path)))
      (obtain-lock rotate-lock)
      (log-set! (open-output-file tmp))
      (if port (close-output-port port))
      (rename cur (clean path))
      (write-ykk-image path)
      (rename tmp cur)
      (release-lock rotate-lock))))

(define (clobber-log)
  (close-port (log-port))
  (log-set! #f))

(define (list-replay-test)
  (replay-log-port (open-input-file "/tmp/log")))
