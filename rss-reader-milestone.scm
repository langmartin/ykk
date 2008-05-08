;;;; "Type Tree"
(define-record-type/primitive :date
  nongenerative: u84accca0-6325-4bcd-b329-6fe6a1b1ba29
  (seconds (type :number)))

(define-record-type/primitive :alist
  nongenerative: ue2026d4b-cfc4-42e6-acd3-9066ca44406d
  (alist (type :pair)))

(define-record-type/primitive :url
  nongenerative: u339c8b5c-2200-4fec-80f3-868a3a5040d2
  (proto (type :symbol))
  (host (type :string))
  (port (type :number))
  (path (type :string))
  (parameters (type :alist)))

(define-record-type/primitive :text
  nongenerative: u0e11aa4d-d182-4894-854f-ab8b6503cac5
  (body (type :string)))

(define-record-type/primitive :feed
  nongenerative: ucb1ca92a-cf7d-4d8b-929f-bfe6523be23d
  (url (type :url))
  (title (type :string))
  (link (type :url))
  (description (type :text)))

(define-record-type/primitive :rss-item
  nongenerative: u1e4520ea-ed79-4199-bcf0-d6a8e415123f
  (guid (type :string))
  (pubDate (type :date))
  (link (type :url))
  (description (type :text)))

;;;; Tree
(define (scanned-top)
  (scan (persistent-symbol 'top)))

(define update-lock (make-lock))

(define (replay-zipped-transcript updated top)
  (error "I don't know what to do here"))

(define (fail-top-update updated-top)
  (open-update-cursor
   (lambda (top k)
     (k (replay-zipped-transcript updated-top top)))))

(define (open-update-cursor receiver)
  (let ((starting-top (persistent-symbol 'top)))
    (receiver (scan starting-top)
              (lambda (top)
                (dynamic-wind
                    (lambda () (obtain-lock update-lock))
                    (lambda ()
                      (if (eq? starting-top (persistent-symbol 'top))
                          (persistent-symbol-set! 'top (scanned->source top))
                          (fail-top-update top)))
                    (lambda () (release-lock update-lock)))))))

;; (define tree (test-rss-parser "http://okmij.org/ftp/rss.xml"))
;; (p (car ((sxpath `(// item)) tree)))

(define (assq- key alist)
  (cond ((assq key alist) => cadr)
        (else #f)))

(define-syntax let-unspec*
  (syntax-rules ()
    ((_ alist ((key default) ...) . body)
     (let* ((key (or (assq- 'key alist) default))
            ...)
       .
       body))))

(define (//item->item-source alist)
  (let-unspec* alist ((description #f) (link #f) (guid #f) (pubDate #f))
    (make-rss-item
     guid
     pubDate
     link
     description)))

(define (assoc-right/rest key lst . test?)
  (let-optionals* test? ((test? equal?))
    (let* ((box #f)
           (filtered
            (pair-fold-right (lambda (lst tail)
                               (let ((x (car lst)))
                                 (if (and (not box) (test? key (car x)))
                                     (begin
                                       (set! box x)
                                       tail)
                                     (if (eq? tail (cdr lst))
                                         lst
                                         (cons x tail)))))
                             '()
                             lst)))
      (values box filtered))))

(assert
 (values->list (assoc-right/rest 'foo '((bar . 2) (foo . 4) (1 2) (foo baz)))) =>
 '((foo baz)
   ((bar . 2) (foo . 4) (1 2))))

(assert
 (let ((lst (list '(bar . 2) '(foo . 4) '(1 2))))
   (eq? lst
        (cadr (values->list (assoc-right/rest 'foov lst))))))

(define (maybe-update-item item update)
  (let-unspec* update ((description #f) (link #f) (guid #f) (pubDate #f))
    (if (and (equal? description (rss-item-description item))
             (equal? link (rss-item-link item))
             (equal? pubDate (rss-item-pubdate item)))
        item
        (make-rss-item guid link pubDate description))))

(define (match-rss-items zipper spec)
  (if (z-end? zipper)
      (values (move 'up zipper) spec)
      (let* ((item (z-item zipper))
             (guid (rss-item-guid item))
             (cur  (car spec))
             (pair rest (assoc-right/rest guid cur eq?)))
        (let ((new (if (not pair)
                       #f
                       (let ((new (maybe-update-item item cur)))
                         (if (eq? item new)
                             #f
                             new)))))
          (match-rss-items (move 'next zipper new) rest)))))

(define (make-update-rss-feed path top update-k)
  (update-k
   (let* ((scanned-z (resolve top path))
          (scanned (z-item scanned-z))
          (xml (test-rss-parser (feed-url scanned))))
     (let* ((new-kids extra (match-rss-items (graph-children scanned) xml)))
       (move 'up
             (replace-children
              scanned
              (fold (lambda (item kids)
                      (add-child (//item->item-source item) kids))
                    new-kids
                    extra))
             scanned-z)))))

(define (test-rss-parser url)
  (let ((mime (http-get->mime url)))
    (call-with-string-input-port
     (duct->string (mime->duct mime))
     (lambda (port)
       (if #f
           (read-line port #f)
           (ssax:xml->sxml port '()))))))

;; (define patpost
;;   (test-rss-parser (parse-url "http://archive.patriotpost.us/patriotpost.xml")))

;;;; Init
(initialize-logging "/Users/lang/tmp/ykk")

;; this only runs once, ever
(if (not (persistent-symbol 'top))
    (persistent-symbol-set!
     'top
     (begin
       (source:root
        (source:node :folder (plist (sticky #f)))))))

(define-record-type/primitive :cron-job
  nongenerative: u58175730-4c1d-41c6-a041-b60576593a81
  (hour (type :list))
  (minute (type :list))
  (weekday (type :list))
  (monthday (type :list))
  (delay-seconds (type :number)))

(define *sleeping-jobs* (make-vector 2))
(define (jobs) (vector-ref *sleeping-jobs* 0))
(define (jobs! val) (vector-set! *sleeping-jobs* 0 val))
(define (jobs-tree) (vector-ref *sleeping-jobs* 1))
(define (jobs-tree! val) (vector-set! *sleeping-jobs* 1 val))

(define (process-cron-jobs)
  (let ((jobs (resolve `(#f crontabs) (top))))
    (sort-list
     (map (lambda (job)
            (let ((current (time-seconds (current-time)))
                  (hour min week day sleep (uncron-job job)))
              (if (pair? delay)
                  (+ current (car delay))
                  )))))

    (modulo
     (+ (time-seconds (current-time)) (* 60 15))
     (* 60 15))
    
    
    )
  )
