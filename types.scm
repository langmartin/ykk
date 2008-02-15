;;; types
(define-record-type rtd/type-definition
  (really-make-type-definition name ancestors defined super priority slots)
  ykk-type?

  ;; definition
  (name type-name)
  (ancestors defined-ancestors)
  (defined defined-slots)

  ;; computed
  (super type-super)
  (priority really-type-priority)
  (pred type-instance-predicate set-type-instance-predicate!)
  (slots type-slots))

(define-record-discloser rtd/type-definition
  (lambda (type)
    `(type ,(type-name type))))

(define (make-type-definition name ancestors slot-defs)
  (initialize!
   (let ((super (list (most-specific-common-superior ancestors))))    
     (really-make-type-definition
      name
      ancestors
      slot-defs
      super
      (compute-priority super)
      (inherit-slots (car super) slot-defs ancestors)))))

(define (initialize! new-type)
  (set-type-instance-predicate! new-type (make-type-instance-predicate new-type))
  new-type)

(define (make-type-instance-predicate type)
  (lambda (foo)
    (and (type-instance? foo)
         (let ((i-type (instance-type foo)))
           (or (identical-type? i-type type)
               (strict-subtype? i-type type))))))

(define :ykk (initialize!
              (really-make-type-definition
               ':ykk '() '() '() 0 '())))

(define :ykk-type rtd/type-definition)

;; MAYBE: could each SUPER possibly be an expression in the
;; module-configuration language?

(define-syntax type-definition
  (syntax-rules ()
    ((_ (super ...) rest ...)
     (type-definition #f (super ...) rest ...))
    ((_ name () rest ...)
     (type-definition name (:ykk) rest ...))
    ((_ name (super ...) (slot-def ...) ...)
     (make-type-definition
      'name
      (list super ...)
      (make-slot-definitions (slot-def ...) ...)))))

(define-syntax define-type
  (syntax-rules ()
    ((_ name (super ...) (slot-def ...) ...)
     (define name (type-definition name (super ...) (slot-def ...) ...)))
    ((_ name (super ...) pred? (slot-def ...) ...)
     (begin
       (define-type name (super ...) (slot-def ...) ...)
       (define pred? (type-instance-predicate name))))))

(define (inherit-slots super defs ancestors)
  (receive (projection rest)      
      (project-slots-onto (merge-slots-by-name (append (map type-slots ancestors) (list defs)))
                          super)
    (append projection rest)))

;; --------------------
;; from methods.scm
(define *increment* 10)

(define (compute-priority supers)
  (if (null? supers)
      0
      (+ (apply max (map %type-priority supers))
	 *increment*)))

(define (more-specific-type? t1 t2)
  (> (%type-priority t1) (%type-priority t2)))
;; --------------------


;;; predicates on types
(define-syntax define-meta-type-predicate
  (syntax-rules ()
    ((_ name meta-pred?)
     (define (name t1 t2)
       (and (meta-pred? t1)
            (meta-pred? t2))))))

(define-meta-type-predicate simple-types? simple-type?)
(define-meta-type-predicate record-types? record-type?)
(define-meta-type-predicate ykk-types? ykk-type?)

(define identical-type? eq?)

;; --------------------
;; strict subtype

;; Strict subtyping is meant in the "Liskov Substitution Principle"
;; sense.  See http://okmij.org/ftp/Computation/Subtyping/

;; Strict subtyping is used to integrate ykk types with generic
;; dispatch.

(define (strict-subtype? t1 t2)
  (and (or (%same-type? t1 t2)
           (direct-descendant? t1 t2))))

;; --------------------
;; ancestor set predicates

(define (direct-descendant? t1 t2)
  (find (cut %same-type? t2 <>) (ancestors t1)))

(define (ancestors<= t1 t2)
  (ancestor-set-pred lset<= %same-type? t1 t2))

(define (ancestors-equal? t1 t2)
  (ancestor-set-pred lset= %same-type? t1 t2))

(define (ancestor-set-pred lset-op pred? t1 t2)
  (lset-op pred?
           (ancestors t1)
           (ancestors t2)))

;; --------------------
;; similar types

;; Can T1 be substituted for T2 in some form without generating any
;; logical-errors if that form is recompiled?

(define (%similar-type? t1 t2)
  (or (identical-type? t1 t2)
      ;; FIXME: temporary kludge while ykk type top-level remains a simple-type
      (and (ykk-type? t1) (identical-type? t2 :ykk))
      (cond ((simple-types? t1 t2) (similar:simple? t1 t2))
            ((record-types? t1 t2) (similar:record? t1 t2))
            ((ykk-types? t1 t2) (similar:ykk? t1 t2))
            (else (similar-type? t1 t2)))))

(define (similar:simple? t1 t2)
  ;; Trust that simple types do not violate a this assertion:
  ;;
  ;;   ((type-predicate t2) instance-of-t1) ==> #t
  ;;
  ;; And, more generally, that INSTANCE-OF-T1 will not generate a run-time error
  ;; when it is passed to a procedure that accepts an argument of type T2.
  (or (identical-type? t1 t2)
      (%same-type? t1 t2)
      (find-superior (cut %same-type? t2 <>)
                     (%type-superiors t1))))

(define (similar:record? t1 t2)
  ;; punt -- record accessors run through checked-record-ref, which
  ;;  will error on any record instance not eq? in type to the one the
  ;;  accessor is defined over.
  (%same-type? t1 t2))

(define (similar:ykk? t1 t2)
  ;; YKK types have typed slots.  Use these, namewise, as the basis of
  ;; type-equality comparison.  Destructuring is based on slot names,
  ;; so slot order and superiors don't matter.
  ;;
  ;; All slot names in T2 must be present in T1.  For each pair of
  ;; matching slots, the slot in T1 must be a similar to the slot in
  ;; T2.
  (or (identical-type? t1 t2)
      (identical-type? t2 :ykk)
      (recursive-slot-type-assertion? %similar-type? t1 t2)))

(define (find-superior pred? supers)
  (breadth-first pred? type-superiors supers))

(define (breadth-first pred? expand set)
  (or (any pred? set)
      (any (cut breadth-first pred? expand <>)
           (map expand set))))

(define-syntax bind-joined
  (syntax-rules (<>)
    ((_ (cut-expr ...) (join-expr ...))
     (let ((joined (join-expr ...)))
       (and joined
            ((cut cut-expr ...) joined))))))

(define (recursive-slot-type-assertion? pred? t1 t2)
  (bind-joined
   (every (joined-pred pred?) <>)
   (join-all-slots->types t1 t2)))

(define (join-all-slots->types more-specific less-specific)
  (bind-joined
   (map-joined slot-type <>)
   (join-all slot-name-eq? (type-slots more-specific) (type-slots less-specific))))

(define (join-all pred? larger-set smaller-set)
  (call-with-current-continuation
   (lambda (return)

     (define (match item result)

       (define (found-it matched)
         (cons (list matched item)
               result))

       (cond ((find (cut pred? item <>) larger-set) => found-it)
             (else (return #f))))

     (fold match '() smaller-set))))

(define (joined-pred pred?)
  (cut apply pred? <>))

(define (map-joined proc joined)
  (map (cut map proc <>) joined))

;;; operations on types
(define (%type-priority t)
  (if (ykk-type? t)
      (really-type-priority t)
      (type-priority t)))

(define (%type-superiors t)
  (cond ((ykk-type? t) (type-super t))
        (else (type-superiors t))))

(define (%same-type? t1 t2)
  (or (identical-type? t1 t2)
      (same-type? t1 t2)))

(define (ancestors-and-self t)
  (cons t (ancestors t)))

(define (ancestors t)
  (delete-duplicates (accumulate-superiors t) %same-type?))

(define (accumulate-superiors t)
  (expanding-fold cons %type-superiors '() (%type-superiors t)))

(define (sort-by-specificity unique-types)
  (insertion-sort more-specific-type? unique-types))

(define (most-specific-common-superior types)
  (cond ((null? types)
         '())        
        ((null? (cdr types))
         (car types))
        (else
         (most-specific-type
          (apply-intersection %same-type? (map ancestors-and-self types))))))

(define (most-specific-type types)
  (car (sort-by-specificity types)))

(define (type-slot-ref type name)
  (find (lambda (slot) (eq? name (slot-name slot)))
        (type-slots type)))

;; --------------------
;; utility

(define (insertion-sort > lst)
  (fold (cut insert > <> <>)
        '()
        lst))

;; insert ITEM into LST, maintaining the invariant
;;
;;   (apply > lst) ==> #t
(define (insert > item lst)
  (if (null? lst)
      (list item)
      (receive (left right)
          (partition (cut > <> item) lst)
        (append left (cons item right)))))

;; depth-first expansion
(define (expanding-fold kons expand seed lst)

  (define (recur item acc)
    ;; minor optimization: check for a non-null expansion before
    ;; making the recursive call.
    (let ((expanded (expand item)))
      (if (null? expanded)
          acc
          (expanding-fold kons expand acc expanded))))

  (fold (lambda (item acc) (kons item (recur item acc)))
        seed
        lst))

(define (apply-intersection pred? sets)
  (apply lset-intersection (cons pred? sets)))

;;; slots
(define-record-type rtd/slot-definition
  (really-make-slot-definition name type initform)
  slot-definition?
  (name slot-name)
  (type slot-type)
  (initform slot-initform))

(define-record-discloser rtd/slot-definition
  (lambda (def)
    `(slot-definition ,(slot-name def) ,(slot-type def))))

(define-syntax define-slot
  (syntax-rules ()
    ((_ name)
     (define-slot name :value))
    ((_ name type)
     (define-slot name type (undefined)))
    ((_ name type initform)
     (really-make-slot-definition 'name type initform))))

(define-syntax make-slot-definitions
  (syntax-rules ()
    ((_ (def ...) ...)
     (slot-definitions->type-definition-slots
      (list (define-slot def ...) ...)))))

;;; operations on slots
(define (project-slots-onto slots type)
  (project-slots-onto-names slots (map slot-name (type-slots type))))

(define (project-slots-onto-names slots names)
  
  (define (find-name slot)
    (memq (slot-name slot) names))

  (define (find-slot name slots)
    (find (lambda (slot)
            (eq? name (slot-name slot)))
          slots))  

  (receive (matched unmatched)
      (partition find-name slots)
    (values (map (cut find-slot <> matched) names)
            unmatched)))

(define (merge-slots-by-name slot-sets)
  (map merge-slot-group (group-slots-by-name slot-sets)))

(define (merge-slot-group slot-group)

  (define (error slot continue-with-value . error-args)
    (nonfatal-continue
     continue-with-value
     (apply type-error `(,@error-args
                         (bad-slot: ,slot)
                         (last-good-value: ,continue-with-value)
                         (group: ,slot-group)))))

  (define (choose-name slot name)
    (if (or (not name) (eq? name (slot-name slot)))
        (slot-name slot)
        (error slot name "merge-slot-group: inconsistent name")))

  (define (choose-type slot type)
    (cond ((not type) (slot-type slot))
          ((strict-subtype? type (slot-type slot)) type)
          (else
           (error slot type "merge-slot-group: not a strict supertype"))))

  (define (choose-initform slot form)
    (if (undefined? form)
        (slot-initform slot)
        form))

  (apply really-make-slot-definition
         (fold (lambda (slot template)
                 (list (choose-name slot (car template))
                       (choose-type slot (cadr template))
                       (choose-initform slot (caddr template))))
               (list #f #f (undefined))
               slot-group)))

(define (group-slots-by-name slot-sets)
  (ordered-groups slot-name-eq?
                  slot-more-specific?
                  slot-sets))

(define (slot-name-eq? s1 s2)
  (eq? (slot-name s1) (slot-name s2)))

(define (slot-more-specific? s1 s2)
  (more-specific-type? (slot-type s1) (slot-type s2)))

;; --------------------
;; utility

(define (ordered-groups = > relations)

  (define (insert-one item groups)
    (cond ((null? groups)
           (cons (list item) groups))
          ((= item (caar groups))
           (cons (insert > item (car groups))
                 (cdr groups)))
          (else
           (cons (car groups)
                 (insert-one item (cdr groups))))))

  (define (group-one rel groups)
    (fold insert-one groups rel))

  (fold group-one '() relations))


;;; type instances
(define-record-type rtd/type-instance
  (make-type-instance type values)
  type-instance?
  (type instance-type)
  (values instance-values))

(define-record-discloser rtd/type-instance
  (lambda (instance)
    `(instance of ,(type-name (instance-type instance)))))

(define-syntax new
  (syntax-rules ()
    ((_ type args ...)
     (make-type-instance
      type
      (fold-slot-values type (list args ...))))))

;; FOLD-SLOT-VALUES -- check the arguments about to be applied to the
;;  constructor, interspersing initforms where necessary.
(define (fold-slot-values type arguments)
  (let loop ((defs (type-slots type))
             (arguments arguments)
             (all-arguments '()))
    (cond ((and (null? arguments)
                (null? defs))
           (reverse all-arguments))
          ((null? defs)
           (nonfatal-continue
            (loop '() '() all-arguments)
            (type-error "fold-slot-values: too many arguments for type"
                        `(arguments ,arguments)
                        type)))
          ((has-init-form? (car defs))
           (loop (cdr defs)
                 arguments
                 (cons (eval-initform (car defs))
                       all-arguments)))
          ((null? arguments)
           (nonfatal-continue
            (loop '() '() all-arguments)
            (type-error "fold-slot-values: missing arguments for type"
                        `(arguments ,(map slot-name defs))
                        type)))
          (else
           (let ((slot (car defs)))
             (loop (cdr defs)
                   (cdr arguments)
                   (cons (assert-type slot (car arguments))
                         all-arguments)))))))

(define (eval-initform form)
  ;; FIXME: punt for now
  form)

(define (assert-type slot value)
  (if ((type-predicate (slot-type slot)) value)
      value
      (nonfatal-continue
       value
       (type-error "assert-type failed: value is not an instance of type"
                   `(value: ,value)
                   `(type: ,(slot-type slot))
                   slot))))

;;;  integration with METHODS
(define simple-type (record-ref :value 0))

(define (simple-type? foo)
  (and (record? foo)
       (eq? (record-ref foo 0) simple-type)))

(define-method &type-predicate ((type rtd/type-definition))
  (type-instance-predicate type))

(define-method &type-superiors ((type rtd/type-definition))
  (type-super type))

(define-method &type-priority ((type rtd/type-definition))
  (really-type-priority type))

(define-generic similar-type? &similar-type? (t1 t2))

(define-method &similar-type? ((t1 :simple-type) (t2 :simple-type))
  (similar:simple? t1 t2))

(define-method &similar-type? ((t1 :record-type) (t2 :simple-type))
  (similar:record? t1 t2))

(define-method &similar-type? ((t1 rtd/type-definition) (t2 rtd/type-definition))
  (similar:ykk? t1 t2))

;;; operations on type instances and slots
(define (undefined) undefined)
(define (undefined? foo) (eq? foo undefined))

(define (has-init-form? slot-def)
  (not (undefined? (slot-initform slot-def))))

(define (slot-definitions->type-definition-slots definitions)
  definitions)

(define (slot-names instance)
  (map slot-name (type-slots (instance-type instance))))

(define (slot-types instance)
  (map (lambda (slot-def)
         (slot-type slot-def))
       (type-slots (instance-type instance))))

;;; auxiliary
(define-condition
  type-error (syntax-error)
  type-error?)

(define-condition
  type-warning (warning)
  type-warning?)

(define-syntax nonfatal-continue
  (syntax-rules ()
    ((_ continue-form warn-form)
     (begin warn-form continue-form))))

;;; tests
(begin
  (assert (similar:simple? :real :number))
  (assert (similar:simple? :number :real) => #f)

  (define-type :food ()
    (name :string))

  (define-type :spice (:food)
    (hot? :boolean))

  (define-type :fruit (:food)
    (tropical? :boolean))

  (define-type :cinnamon (:spice)
    (hot? :boolean #t))

  (define-type :apple (:fruit)
    (tropical? :boolean #f))

  (define-type :pie (:apple :cinnamon)
    (name :string "apple-cinnamon"))  

  (define-type :pastry (:food)
    (hot? :boolean)
    (frosting-flavor :symbol))

  (assert (similar-type? :pastry :spice))
  (assert (not (similar-type? :pie :pastry)))

  (assert (direct-descendant? :pie :food))
  (assert (not (direct-descendant? :pie :apple)))

  (assert (strict-subtype? :pie :food))
  (assert (not (strict-subtype? :pastry :cinnamon)))
  (assert (not (strict-subtype? :pastry :pie)))

  (define-generic cook &cook)
  (define-method &cook (v) "I don't know how")
  (define-method &cook ((v :apple)) "roast it")

  (assert (cook (new :pie)) => "I don't know how")
  (assert (cook (new :apple "apple sauce")) => "roast it")
  (assert (cook (new :pastry "cinnamon roll" #t 'caramel)) => "I don't know how")

  )