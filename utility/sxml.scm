
(define (path-run make path nodes)
  (let ((path (if (procedure? path) path (make path))))
    (path nodes)))

(define (sxpath-run path nodes)
  (path-run sxpath path nodes))

(define (txpath-run path nodes)
  (path-run txpath path nodes))

(define-syntax let-sxml-attrs
  (syntax-rules ()
    ((_ item (attrs keys ...) expr ...)
     (let* ((attrs (sxml:attr-list-u item))
            (keys ... (bind-spec (keys ...) attrs)))
       expr ...))))

(define-syntax let-sxml-pluck-attrs
  (syntax-rules ()
    ((_ item (attrs keys ...) expr ...)
     (let* ((attrs (sxml:attr-list-u item))
            (keys ... attrs (pluck-spec (keys ...) attrs)))
       expr ...))))

;(define test '(div (@ (class "foo") (width "90")) "some text"))
