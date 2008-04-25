
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

(define-syntax bind-attributes
  (syntax-rules ()
    ((_ names attributes)
     (bind-spec names (remove-@ attributes)))))

(define-syntax pluck-attributes
  (syntax-rules ()
    ((_ names attributes)
     (pluck-spec names (remove-@ attributes)))))

;; ADD-CLASS is a restricted form of UPDATE-ATTRIBUTES that only
;; affects the `class' attribute.
(define (add-class attributes classes)
  (update-attributes
   attributes
   `((class ,classes))))

;; JOIN-CLASSES is a stricter form of MERGE-CLASS-SETS that returns a
;; class-string instead of a list of classes.
(define (join-classes . sets)
  (string-join (apply merge-class-sets sets)))

;; MERGE-CLASS-SETS takes a list of class sets and merges them
;; together.  Each set can be a string of one or more classes (i.e. "a
;; b c") or a list of strings (i.e. '("a" "b" "c")).  The result is a
;; unique list of classes in the order they first appear in SETS from
;; left to right.
(define merge-class-sets
  (let ((kons (unique-conser string=?)))
    (lambda sets
      (reverse
       (apply fold-append
              kons
              '()
              (map-in-order maybe-tokenize sets))))))

;; UPDATE-ATTRIBUTES is a "canonical" attribute list merger.  It takes a
;; number of attribute SETS and combines them.  Subsequent attributes
;; replace previous attributes with the same name.  The exception is
;; the `class' attribute which is merged using JOIN-CLASSES.
(define (update-attributes . sets)
  (cons '@
   (apply attribute-updater
          (map-in-order remove-@ sets))))

(define attribute-updater
  (let ((template `((class ,join-classes))))
    (lambda sets
      (apply merge-attribute-sets template sets))))

;; MERGE-ATTRIBUTE-SETS is an abstract attribute list merger.  It
;; takes a TEMPLATE that can specifiy how to merge certain attributes.
;; The default behavior is for subsequent attributes to replace
;; previous attributes with the same name.
(define (merge-attribute-sets template . sets)
  (apply merge-alists/template
         list
         cadr
         template
         proj-1
         sets))

(define (remove-@ set)
  (if (and (pair? set)
           (eq? (car set) '@))
      (cdr set)
      set))

;;;; Tests
;(define test '(div (@ (class "foo") (width "90")) "some text"))
(begin
  (let ((a b (bind-attributes (a b) `(@ (a 1) (b 2) (c 3)))))
    (assert a => 1)
    (assert b => 2))

  (let ((c rest (pluck-attributes (c) `(@ (a 1) (b 2) (c 3)))))
    (assert c => 3)
    (assert rest => '((a 1) (b 2))))

  (assert (merge-class-sets "foo bar" "bar baz" "bum foo quux")
          => '("foo" "bar" "baz" "bum" "quux"))

  (assert (join-classes "a b" "c")
          => "a b c")

  (assert (add-class '(@ (class "foo bar")) "baz bar")
          => '(@ (class "foo bar baz")))

  (assert
   (update-attributes
    '(@ (a 1) (class "foo"))
    '(@ (a 2) (b 3) (class "bar")))
   => '(@ (a 2) (class "foo bar") (b 3)))    
  )

