
(define-condition sxpath-error (error) sxpath-error?)

(define (sxpath-run expr nodes)
  ((sxpath expr) nodes))

(define (sxml-attlist node)
  (sxpath-run `(@ *) node))

(define-syntax let-sxml-attlist
  (syntax-rules ()
    ((_ item (attrs keys ...) expr ...)
     (let* ((attrs (sxml-attlist item))
            (keys ... (bind-spec (keys ...) attrs)))
       expr ...))))

(define-syntax let-sxml-pluck-attlist
  (syntax-rules ()
    ((_ item (attrs keys ...) expr ...)
     (let* ((attrs (sxml-attlist item))
            (keys ... attrs (pluck-spec (keys ...) attrs)))
       expr ...))))

(define (sxml-first-text node)
  (and-let* ((xp-query (sxpath-run `(// *text*) node))
             (null? xp-query))
    (car xp-query)))

;; Tests

(define *test-element* '(div (@ (class "outer"))
                             (div "foo content")
                             "bar content"
                             (div "footer")))

(assert (sxpath-run '(// div) *test-element*)
        => '((div "foo content") (div "footer")))

(assert (sxml-attlist *test-element*)
        => '((class "outer")))

(assert (sxml-first-text *test-element*)
        => "bar content")

(assert (let-sxml-pluck-attlist *test-element* (attrs class bad)
          (list class bad))
        => '("outer" #f))
