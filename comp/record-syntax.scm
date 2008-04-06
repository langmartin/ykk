;;;; Universal getter / setter
(define-syntax unrecord
  (lambda (form rename compare)
    (let ((rec (cadr form))
          (type (environment-ref (interaction-environment) (caddr form)))
          (slot-names (cdddr form)))

      (define %values (rename 'values))
      (define %stob-ref (rename 'stob-ref))

      (define (make-ref name)
        `(,%stob-ref ,rec ,(type-slot-index type (desyntaxify name))))

      `(,%values ,@(map-in-order make-ref slot-names)))))

;; Note use of GENERATIVE-MAKE here
(define-syntax record-update
  (lambda (form rename compare)
    (let* ((rec (cadr form))
           (type-name (caddr form))
           (type (environment-ref (interaction-environment) type-name))
           (slots (map-car desyntaxify (cdddr form)))
           (keep-names (map-in-order
                        specification-name
                        (remove
                         (lambda (spec)
                           (assq (specification-name spec) slots))
                         (description-specifications (rtd-slots type))))))

      (define %let* (rename 'let*))
      (define %values (rename 'values))
      (define %unrecord (rename 'unrecord))
      (define %make (rename 'generative-make))

      `(,%let*
        ((,@(map-in-order car slots) (,%values ,@(map-in-order cadr slots)))
         (,@keep-names (,%unrecord ,rec ,type-name ,@keep-names)))
        (,%make ,@(specification-names (rtd-slots type))))
      )))

;;;; Primitive Syntactic Form
(define-syntax* (define-record-type/primitive
                  define-name
                  (predicate: predicate #f)
                  (name: name #f)
                  (parent: parent #f)
                  (nongenerative: uid #f)
                  (sealed: sealed? #f)
                  (opaque: opaque? #f)
                  (constructor: constructor #f)
                  (protocol: protocol #f)
                  (nongenerative-parameter: nongenerative-formal #f)
                  . slots)
  (syntax/normalize-description
   slots
   (expand-primitive-definitions
    (define-name predicate)
    (make-rtd-type name parent uid sealed? opaque?)
    (protocol nongenerative-formal constructor))))

(define-syntax expand-primitive-definitions
  (syntax-rules ()
    ((_ slots
        (define-name predicate-name)
        (make name parent uid sealed? opaque? . more)
        (protocol nongenerative-formal constructor))
     (begin
       (define/expansion define-name
         (make (or 'name (define-name->name 'define-name))
           parent
           'uid
           sealed?
           opaque?
           (description slots)
           . more))
       (expand-predicate-definition define-name predicate-name)
       (expand-protocol-definition define-name nongenerative-formal constructor protocol slots)
       (expand-accessor-definitions define-name predicate-name slots)
       ))))

(define-syntax expand-predicate-definition
  (lambda (form rename compare)
    (let* ((type-name (cadr form))
           (type (environment-ref (interaction-environment) type-name))
           (predicate-name (caddr form)))

      (define %define (rename 'define))
      (define %rtd-predicate (rename 'rtd-predicate))

      `(,%define ,(or predicate-name (rtd->predicate-name type))
         (,%rtd-predicate ,type-name)))))

(define-syntax expand-protocol-definition
  (lambda (form rename compare)
    (let* ((type-name nongenerative-formal constructor-name protocol slots (unlist (cdr form)))
           (type (environment-ref (interaction-environment) type-name))
           (nongenerative? (and nongenerative-formal #t))
           (parent (rtd-parent type))
           (protocol-name (rtd->protocol-name type))
           (driver-name (rtd->driver-name type))
           (constructor-name (or constructor-name (rtd->constructor-name type))))

      (define %begin (rename 'begin))
      (define %define (rename 'define))
      (define %make-protocol-driver (rename 'make-protocol-driver))
      (define %environment-ref (rename 'environment-ref))
      (define %rtd-environment (rename 'rtd-environment))
      (define %rtd-parent (rename 'rtd-parent))
      (define %always? (rename 'always?))
      (define %lambda (rename 'lambda))
      (define %let (rename 'let))
      (define %let* (rename 'let*))
      (define %and (rename 'and))
      (define %unrecord (rename 'unrecord))

      (define (reify-protocol)
        (cond (protocol protocol)
              (nongenerative? (reify-nongenerative))
              (else (reify-generative))))

      ;; see DEFAULT-NONGENERATIVE-PROTOCOL
      (define (reify-nongenerative)
        (let* ((%p (rename (uuidgen)))
               (%c (rename (uuidgen)))
               (init (these-initializers))
               (description (rtd-slots type))
               (formals (uninitialized description)))
          `(,%lambda (,%p)
            (,%lambda ,formals
             ,(if parent
                  (let* ((parent-d (rtd-slots parent))
                         (parent-formals (uninitialized parent-d))
                         (for-parent for-these (split-initializers parent-formals init))
                         (these (not-in-parent parent slots)))
                    (maybe-initialize
                     for-parent
                     `((,%p ,@parent-formals)
                       ,nongenerative-formal
                       (,%lambda (,%c)
                        ,(maybe-initialize
                          for-these
                          `(,%c ,@these)))
                       ,(reify-verifier)
                       #f)))
                  `(,%p ,nongenerative-formal
                        (,%lambda (,%c)
                         ,(maybe-initialize
                           init
                           `(,%c ,@(specification-names description))))
                        ,(reify-verifier)
                        #f))))))

      (define (split-initializers parent-formals init)
        (let loop ((init (reverse init)) (for-parent '()) (for-these '()))
          (cond ((null? init)
                 (values (reverse for-parent)
                         for-these))
                ((memq (caar init) parent-formals)
                 (loop '()
                       init
                       for-these))
                (else
                 (loop (cdr init)
                       for-parent
                       (cons (car init)
                             for-these))))))

      (define (reify-verifier)
        (let ((verifiers (attributes slots '(equal))))
          (if (null? verifiers)
              %always?
              (let ((%r (rename (uuidgen))))
                `(,%lambda (,%r)
                  (,%and
                   ,@(map-in-order
                      (lambda (verify)
                        (let* ((slot-name proc (unlist verify)))
                          `(,proc ,slot-name (,%unrecord ,%r ,type-name ,slot-name))))
                      verifiers)))))))

      ;; see DEFAULT-GENERATIVE-PROTOCOL
      (define (reify-generative)
        (let* ((%p (rename (uuidgen)))
               (init (these-initializers))
               (description (rtd-slots type))
               (formals (uninitialized description)))
          `(,%lambda (,%p)
            (,%lambda ,formals
             ,(maybe-initialize
               init
               (if parent
                   (let* ((parent-d (rtd-slots parent))
                          (parent-formals (uninitialized parent-d))
                          (these (not-in-parent parent slots)))
                     `((,%p ,@parent-formals) ,@these))
                   `(,%p ,@(specification-names description))))))))

      (define (maybe-initialize init body)
        (if (null? init)
            body
            `(,%let* ,init ,body)))

      (define (attributes specs names)
        (project-specifications specs cadr project-specification names))

      (define (these-initializers)
        (map-in-order
         (lambda (item)
           (let ((name init commit (unlist item)))
             (list name init)))
         (project-specifications slots anything? project-specification '(init commit))))

      (define (anything? item)
        (any identity (cdr item)))

      (define (uninitialized description)
        (map-in-order car (uninitialized-specs description)))

      (define (uninitialized-specs description)
        (project-description
          description
          (lambda (item)
            (let ((name init commit type (unlist item)))
              (not (or init commit))))
          project-specification
          '(init commit type)))

      (define (not-in-parent parent specs)
        (let ((in-parent (specification-names (rtd-slots parent)))
              (names (map-in-order specification-name specs)))
          (lset-difference eq? names in-parent)))

      (define (reify-driver)
        (let ((%compose (rename (if nongenerative? 'nongenerative-composer 'generative-composer)))
              (%make (rename (if nongenerative? 'nongenerative-make 'generative-make))))
          `(,%make-protocol-driver
            ,(and parent (reify-parent-env-ref (rtd->protocol-name parent)))
            (,%compose
             ,(if parent
                  (reify-parent-env-ref (rtd->driver-name parent))
                  %make)
             ,type-name
             ,%always?))))

      (define (reify-constructor)
        (let ((%make (rename (if nongenerative? 'basic-nongenerative-constructor 'basic-generative-constructor))))
          `(,%make ,driver-name ,protocol-name)))

      (define (reify-parent-env-ref name)
        `(,%environment-ref
          (,%rtd-environment (,%rtd-parent ,type-name))
          ',name))

      `(,%begin
        (,%define ,protocol-name ,(reify-protocol))
        (,%define ,driver-name ,(reify-driver))
        (,%define ,constructor-name ,(reify-constructor))
        ))))

(define-syntax expand-accessor-definitions
  (lambda (form rename compare)
    (let* ((type-name predicate-name slots (unlist (cdr form)))
           (type (environment-ref (interaction-environment) type-name))
           (parent (rtd-parent type)))

      (define %define-stob-accessors (rename 'define-stob-accessors))

      (define (not-in-parent specs)
        (let ((in-parent (specification-names (rtd-slots parent))))
          (remove
           (lambda (spec)
             (memq (specification-name spec) in-parent))
           specs)))

      (let ((names (if parent
                       (not-in-parent slots)
                       slots)))

        `(,%define-stob-accessors
          ,(or predicate-name (rtd->predicate-name type))
          ,@(map-in-order
             (lambda (spec)
               `(,(rtd-accessor-name type spec)
                 ,(type-slot-index type (specification-name spec))))
             names))))))

;;;; DEFINE-RECORD-TYPE
;; FIXME