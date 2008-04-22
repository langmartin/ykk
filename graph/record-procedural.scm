;;;; Protocol
(define (make-protocol-driver extends-protocol compose)
  (if extends-protocol
      (extending-driver extends-protocol compose)
      (basic-driver compose)))

(define (basic-driver compose)
  (lambda extension-values
    (lambda these-values
      (compose these-values extension-values))))

(define (extending-driver previous-protocol compose)
  (lambda extension-values
    (lambda previous-protocol-values
      (lambda these-values
        (apply (previous-protocol
                (compose these-values extension-values))
               previous-protocol-values)))))

(define (length-validator type len)
  (lambda (v)
    (or (= (length v) len)
        (error "wrong number of arguments to constructor"
               type
               v)
        #f)))

(define (verifier verify? . args)
  (lambda (r) (apply verify? r args)))

;;;; Generative
(define (generative-allocator commit . args)
  (allocate/verify
   #f
   (lambda () (apply make-stob args))
   never?
   commit))

(define (generative-make type . rest)
  (apply generative-allocator
         identity
         (if (vector? type) type (list->vector type))
         rest))

(define (generative-no-extension driver)
  (driver))

(define (generative-composer make type valid?)
  (lambda (these ext)
    (if (valid? these)
        (compose-typed-argument-lists make type these ext))))

(define (compose-typed-argument-lists make type a b)
  (if (null? b)
      (apply make (list type) a) 
      (apply make (cons type (car b)) (append a (cdr b)))))

(define (basic-generative-constructor driver protocol)
  (protocol (generative-no-extension driver)))

(define (default-generative-protocol previous-arg-count)
  (if (not previous-arg-count)
      (lambda (p)
        (lambda args
          (apply p args)))
      (lambda (p)
        (lambda args
          (let ((previous-args these-args (split-at args previous-arg-count)))
            (apply (apply p previous-args) these-args))))))

;;;; Non-generative Protocol
(define (collect-null c) (c))

(define (make-nongenerative-stob type . rest)
  (apply make-stob
         (if (vector? type) type (list->vector type))
         rest))

(define (nongenerative-allocator static-identifier collect verify? commit)
  (allocate/verify
   static-identifier
   (lambda () (apply make-nongenerative-stob (collect (lambda args args))))   
   (default-nongenerative-verifier verify?)
   (or commit identity)))

(define nongenerative-make nongenerative-allocator)

(define (default-nongenerative-verifier verify?)
  (lambda (r)
    (cond ((eq? verify? never?)
           (warn 'nongenerative-make
                 "ignoring new definition; keeping original"
                 r))
          ((not (verify? r))
           (warn 'nongenerative-make
                 "inconsitent static object; keeping original"
                 r)))
    r))

(define (nongenerative-no-extension driver)
  (driver #f collect-null always? #f))

(define (nongenerative-composer make type valid?)
  (lambda (these ext)
    (let ((these-id make-these verify-these commit-these (unlist these))
          (ext-id make-ext verify-ext commit-ext (unlist ext)))
      (make (or these-id ext-id)
        (lambda (c)
          (let ((these (make-these c)))
            (if (valid? these)
                (compose-typed-argument-lists c type these (make-ext c)))))
        (lambda (r)
          (and (verify-these r) (verify-ext r)))
        (cond ((and commit-these commit-ext)
               (lambda (s)
                 (commit-ext (commit-these s))))
              (commit-these commit-these)
              (commit-ext commit-ext)
              (else #f))))))

(define (basic-nongenerative-constructor driver protocol)
  (protocol (nongenerative-no-extension driver)))

(define (default-nongenerative-protocol previous-arg-count consistent?)
  (if (not previous-arg-count)
      (lambda (k)
        (lambda (id . args)
          (k id
             (lambda (c) (apply c args))
             (apply verifier consistent? args)
             #f)))      
      (lambda (p)
        (lambda args
          (let ((previous-args these-args (split-at args previous-arg-count)))
            (let ((n (apply p previous-args)))
              (n #f
                 (lambda (c) (apply c these-args))
                 (apply verifier consistent? these-args)
                 #f)))))))

;;;; MOP Extensions
(define-generic type-slot-index &type-slot-index (rtd name))

(define (calculate-priority parent)
  (+ (if parent (type-priority parent) 0)
     10))

;;;; Predicate / Accessors
(define-stob-accessors
  (stob-type 0))

(define-syntax guarantee
  (syntax-rules ()
    ((_ rtd foo index)
     (and (stob? foo)
          (let ((v (stob-type foo)))
            (and (vector? v)
                 (> (vector-length v) index)
                 (eq? (vector-ref v index) rtd)))))))

(define (make-record-predicate rtd)
  (if (vector? rtd)
      (type-vector->predicate rtd)
      (type-vector->predicate (rtd->type-vector rtd))))

(define (rtd->type-vector rtd)
  (list->vector
   (reverse
    (unfold (lambda (rtd) (not (unchecked-rtd-parent rtd)))
            identity
            unchecked-rtd-parent
            rtd
            list))))

(define (type-vector->predicate v)
  (let* ((index (- (vector-length v) 1))
         (type (vector-ref v index)))    
    (lambda (foo)
      (guarantee type foo index))))

(define (make-record-accessor rtd k)
  (let ((index (type-slot-index rtd k)))
    (lambda (foo)
      (stob-ref foo index))))

;;;; Syntax
(define-syntax description
  (syntax-rules ()
    ((_ specification)
     (type-description
      specification
      ((init (syntax/quote-non-literal))
       (commit (syntax/quote-non-literal)))))
    ((_ specification k)
     (syntax-k (description specification) k))))

;;;; Record Type Descriptor

;; --------------------
;; Bootstrap

(define (maybe-rtd? foo)
  (or (not foo)
      (guarantee :rtd-type foo 0)))

(define-simple-type :maybe-rtd (:record :boolean) maybe-rtd?)

(define (describe/predicate-self s)
  (let ((v (rtd->type-vector s)))    
    (stob-set! s 0 v)
    (stob-set! s 10 (make-record-predicate v)))
  s)

(define-stob-accessors
  (unchecked-rtd-parent 3))

(define :rtd-type
  ((lambda (static-id slots)
     (nongenerative-allocator
      static-id
      (lambda (c)
        (c '(type-goes-here)
           (new-identifier)
           'rtd-type
           #f
           static-id
           #f
           #f
           slots
           (length (description-specifications slots))
           (interaction-environment)
           #f
           (calculate-priority #f)))
      never?
      describe/predicate-self))   
   'uEA03CFAC-FB21-41AD-896D-321BA9DD9870
   (description
    ((id            (type :symbol)        (init (new-identifier)))
     (name          (type :symbol)        (equal eq?))
     (parent        (type :maybe-rtd)     (equal eq?))
     (nongenerative (type :maybe-symbol))
     (sealed?       (type :boolean)       (equal eq?))
     (opaque?       (type :boolean)       (equal eq?))
     (slots         (type :description)   (init (inherit-slots parent slots)) (equal descriptions-equal?) (formal #t))
     (slot-count    (type :integer)       (init (length (description-specifications slots))))
     (env           (type :package)       (init (interaction-environment)))
     (predicate     (type :procedure)     (commit (make-record-predicate <>)))
     (priority      (type :number)        (init (calculate-priority parent)))))))

;; --------------------
;; Predicates / Accessors

(define rtd-type?
  (stob-ref :rtd-type 10))

(define-stob-accessors
  rtd-type?
  (rtd-id            1)
  (rtd-name          2)
  (rtd-parent        3)
  (rtd-nongenerative 4)
  (rtd-sealed?       5)
  (rtd-opaque?       6)
  (rtd-slots         7)
  (rtd-slot-count    8)
  (rtd-environment   9)
  (rtd-predicate     10)
  (rtd-priority      11))

;; --------------------
;; MOP Integration

;; First, bootstrap :RTD-TYPE as a type-type by using a simple type.
;; This is simpler than manipulating the method-info tables
;; &TYPE-PRIORITY and &TYPE-PREDICATE directly.
  
(define-simple-type :simple-rtd-type (:simple-type :record-type :stob) rtd-type?)

(define-method &type-predicate ((rtd :simple-rtd-type))
  (rtd-predicate rtd))

(define *simple-rtd-type-priority* (type-priority :simple-rtd-type))

(define-method &type-priority ((rtd :simple-rtd-type))
  (+ *simple-rtd-type-priority* (rtd-priority rtd)))

(type-priority :rtd-type)
(type-predicate :rtd-type)

;; :RTD-TYPE is now bootstrapped, finish the integration

(define-method &type-superiors ((rtd :rtd-type))
  (let ((parent (rtd-parent rtd)))
    (if parent
        (list parent)
        '())))

(define-method &type-slot-index ((rtd :rtd-type) name)
  (index-offset
   (description-specification-index (rtd-slots rtd) name)))

(define-method &type-name ((type :rtd-type))
  (rtd-name type))

;; INDEX-OFFSET accounts for fields not decribed by the description in
;; rtd-fields.
(define (index-offset i)
  (+ i 1))

(define-method &disclose ((obj :rtd-type))
  `(record-type ,(rtd-name obj) in ,(rtd-environment obj)))

(define (instance? foo)
  (and (stob? foo)
       (vector? (stob-type foo))))

(define-simple-type :instance (:stob) instance?)

(define-method &disclose ((obj :instance))
  `(record))

;; --------------------
;; MOP un-integration

;; (define (method-table-methods t)  
;;   (record-ref t 1))

;; (define (set-table-methods! t m)
;;   (record-set! t 1 m))

;; (define (method-info-signature info)
;;   (record-ref info 1))

;; (define simple-type? (type-predicate :simple-type))

;; (define (simple-type-name t)
;;   (record-ref t 4))

;; (define (remove-table-methods pred t)
;;   (remove pred (method-table-methods t)))

;; (define (method-bootstrap-pred? info)
;;   (any (lambda (sig)
;;          (cond ((simple-type? sig)
;;                 (eq? ':simple-rtd-type (simple-type-name sig)))
;;                ((stob? sig) #t)
;;                (else #f)))       
;;        (method-info-signature info)))

;; (define (unintegrate t)
;;   (set-table-methods! t (remove-table-methods method-bootstrap-pred? t)))

;; (for-each
;;  unintegrate
;;  (list &type-predicate &type-priority &type-superiors &type-slot-index))

;; --------------------
;; Constructor

(define (rtd-consistent? r name parent sealed? opaque? slots)
  (and (eq? name (rtd-name r))
       (eq? parent (rtd-parent r))
       (eq? sealed? (rtd-sealed? r))
       (eq? opaque? (rtd-opaque? r))
       (descriptions-equal? (inherit-slots parent slots) (rtd-slots r))))

(define (rtd-type-protocol p)
  (lambda (name parent uid sealed? opaque? slots)
    (p uid
       (lambda (c)
         (c (new-identifier)
            name parent uid sealed? opaque?
            (inherit-slots parent slots)
            (description-length slots)
            (interaction-environment)
            'predicate-goes-here
            (calculate-priority parent)))
       (verifier rtd-consistent? name parent sealed? opaque? slots)
       predicate-self)))

(define (predicate-self s)
  (stob-set! s 10 (make-record-predicate s))
  s)

(define rtd-type-driver
  (make-protocol-driver
   #f
   (nongenerative-composer nongenerative-make :rtd-type always?)))

(define make-rtd-type
  (basic-nongenerative-constructor rtd-type-driver rtd-type-protocol))

;;;; Descriptor Operations
(define (inherit-slots parent slots)
  (if (not parent)
      slots
      (combine-slot-descriptors
       (rtd-slots parent)
       slots)))

(define (combine-slot-descriptors d1 d2)
  (combine-descriptions-by-name
   `((type ,(cut choose-more-specific-type d1 <> d2 <>)))
   proj-1
   d1
   d2))

(define (choose-more-specific-type d-a a d-b b)
  (if (same-type? a b)
      a
      (let ((less more (if (more-specific-type? b a) (values b a) (values a b))))
        (if (strict-subtype? less more)
            more
            (error 'incompatible-slot-descriptions
                   `(,more is-not-a-subtype-of ,less)
                   `(original-spec: ,(description-specifications d-a))
                   `(extended-by: ,(description-specifications d-b)))))))

(define (more-specific-type? t1 t2)
  (> (type-priority t1) (type-priority t2)))

;;;; Syntax Helpers
(define (define-name->name name)
  (let* ((s (symbol->string name))
         (size (string-length s)))
    (if (char=? #\: (string-ref s 0))
        (string->symbol (substring s 1 size))
        name)))

(define (rtd->name rtd . suffix)
  (apply concatenate-symbol
         (if (rtd-type? rtd)
             (rtd-name rtd)
             rtd)
         suffix))

(define (rtd->predicate-name rtd)
  (rtd->name rtd '?))

(define (rtd->protocol-name rtd)
  (rtd->name rtd '- 'protocol))

(define (rtd->driver-name rtd)
  (rtd->name rtd '- 'driver))

(define (rtd->constructor-name rtd)
  (concatenate-symbol 'make- (rtd->name rtd)))

(define (rtd-accessor-name rtd spec)
  (cond ((get-specification-attribute spec 'accessor eq?)
         => attribute-value)
        (else
         (rtd->name rtd '- (specification-name spec)))))

;;;; Tests
(begin
  
  ;; --------------------
  ;; Generative

  (let* ((base-p (default-generative-protocol #f))
         (base (make-protocol-driver
                #f
                (generative-composer generative-make 'rtd (length-validator 'rtd 3))))
         (base-c (basic-generative-constructor base base-p))
         (ext-p (default-generative-protocol 3))
         (ext   (make-protocol-driver
                 base-p
                 (generative-composer base 'rtd-ext (length-validator 'rtd-ext 1))))
         (ext-c (basic-generative-constructor ext ext-p)))

    (assert (stob->list (base-c 1 2 3)) => '(#(rtd) 1 2 3))
    (assert (stob->list (ext-c 1 2 3 4)) => '(#(rtd rtd-ext) 1 2 3 4)))

  ;; --------------------
  ;; Nongenerative

  (let* ((base-p (default-nongenerative-protocol #f always?))
         (base (make-protocol-driver
                #f
                (nongenerative-composer nongenerative-make 'rtd (length-validator 'rtd 2))))
         (base-c (basic-nongenerative-constructor base base-p))
         (ext-p (default-nongenerative-protocol 3 always?))
         (ext   (make-protocol-driver
                 base-p
                 (nongenerative-composer base 'rtd-ext (length-validator 'rtd-ext 1))))
         (ext-c (basic-nongenerative-constructor ext ext-p)))

    (assert (stob->list (base-c 'base-c-static-id 2 3)) => '(#(rtd) 2 3))
    (assert (stob->list (ext-c 'ext-c-static-id 2 3 4)) => '(#(rtd rtd-ext) 2 3 4))
    (stob->list (ext-c #f 2 3 4)))

  ;; --------------------
  ;; Syntax Helpers

  (assert (define-name->name 'foo) => 'foo)
  (assert (define-name->name ':foo) => 'foo)  
  )
