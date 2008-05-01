;;;; "Type Tree"
(define-record-type/primitive :alist
  nongenerative: ue2026d4b-cfc4-42e6-acd3-9066ca44406d
  (alist (type :pair)))

(define-record-type/primitive :url
  nongenerative: ue2026d4b-cfc4-42e6-acd3-9066ca44406f
  (proto (type :symbol))
  (host (type :string))
  (port (type :integer))
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

(define-record-type/primitive :cron-job
  nongenerative: u58175730-4c1d-41c6-a041-b60576593a81
  (hour (type :string))
  (minute (type :string))
  (days (type :list))
  (last (type :date)))

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

(define-syntax let-alist
  (syntax-rules ()
    ((_ ((key ... alist)) body ...)
     (let ((key ... (bind-alist (key ...) alist)))
       body ...))))

(define (//item->item-source alist)
  (let-alist ((description link guid pubDate alist))
    (make-rss-item
     guid
     pubDate
     link
     description)))

(define (assoc-right/rest key lst . test?)
  (let-optionals* test? ((test? equal?))
    (let* ((box '())
           (filtered
            (pair-fold-right (lambda (lst tail)
                               (let ((x (car lst)))
                                 (if (and (null? box) (test? key (car x)))
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

(define (match-rss-items zipper sxml-spec)
  (let lp ((zipper zipper) (spec sxml-spec))
    (let* ((guid (rss-item-guid (z-curr-node zipper)))
           (pair rest (assoc-right/rest guid sxml-spec)))
      ))
)

(define (make-update-rss-feed path top update-k)
  (update-k
   (let* ((scanned-z (resolve top path))
          (scanned (z-item scanned-z))
          (xml (test-rss-parser (feed-url scanned))))
     (move
      'up
      (replace-children
       scanned
       (fold (lambda (item new)
               (add-child (//item->item-source item)
                          new))
             (graph-children scanned)
             ((sxpath `(// item)) xml)))
      scanned-z))))

(define (test-rss-parser url)
  (let ((mime (http-get->mime url)))
    (call-with-string-input-port
     (duct->string (mime->duct mime))
     (lambda (port)
       (if #f
           (read-line port #f)
           (ssax:xml->sxml port '()))))))

;;;; Init
(initialize-logging "/Users/lang/tmp/ykk")

;; this only runs once, ever
(if (not (persistent-symbol 'top))
    (persistent-symbol-set!
     'top
     (begin
       (source:root
        (source:node :folder (plist (sticky #f)))))))

(define (find-next-cron-job)
  (let ((jobs (resolve `(#f crontabs) (scanned-top))))
    
    )
  )
