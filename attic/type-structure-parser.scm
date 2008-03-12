;;; some syntax
(define (identity foo) foo)

(define-fluid ($renamer identity)
  renamer with-renamer let-renamer)

(define (rename symbol)
  ((renamer) symbol))

(define-fluid ($path '())
  path with-path let-path)

(define (add-to-path name)
  (if (scheme-binding? name)
      (path)
      (cons name (path))))

(define-syntax descend
  (syntax-rules ()
    ((_ slot body ...)
     (let-path (add-to-path (slot-name slot))
               body ...))))

;;; public interface
(define (structure->grouped-bindings rename form)
  (receive (names bindings)
      (parse rename form '() '())
    (receive (descended-through only-referenced)
        (partition descended-through? (reverse bindings))        
      (values (reverse names)
              descended-through
              only-referenced))))

(define (parse rename form names bindings)
  (let-renamer rename
               (start-parsing form names bindings)))

(define (alias-everything structure)
  (cond ((identifier? structure)
         `(,structure => _))
        ((aliased? structure)
         structure)
        ((nested? structure)
         (descend (structure-name structure)                      
                  (cons (structure-name structure)
                        (map alias-everything (structure-slots structure)))))
        (else
         (parse-error "alias-everything: unrecognized structure" structure))))

;;; internal interface
(define (start-parsing structure names bindings)
  (nested (scheme-binding) names bindings structure))

(define (parse-structure structure names bindings)
  (let ((instance (structure-name structure)))
    (fold2 (lambda (slot names bindings)
             (cond ((identifier? slot)
                    (simple-identifier instance names bindings slot))
                   ((aliased? slot)
                    (simple-aliased instance names bindings slot))
                   ((nested? slot)
                    (nested instance names bindings slot))                   
                   (else
                    (parse-error "parse-structure: malformed structure" slot))))           
           names
           bindings
           (structure-slots structure))))

;;; parse case handlers
(define (simple-identifier instance names bindings slot)
  ;; example: slot-name
  ;; simple identifier: just extract this slot
  ;; into a binding and add it to the namespace
  (let* ((name slot)
         (info (bind-reference name slot instance)))
    (values (cons name names)
            (cons info bindings))))

(define (simple-aliased instance names bindings slot)
  ;; example: (slot-name => alias)
  ;; aliased slot name: extract this slot into a
  ;; binding and the namespace with the given
  ;; name
  (let* ((aliased (make-alias slot))
         (info (bind-reference aliased slot instance)))    
    (values (cons aliased names)
            (cons info bindings))))

(define (nested instance names bindings structure)
  (let ((next-instance (structure-name structure)))    
    (cond ((or (identifier? next-instance)
               (scheme-binding? next-instance))
           (nested-unbound instance names bindings next-instance (structure-slots structure)))
          ((aliased? next-instance)
           (nested-aliased instance names bindings next-instance (structure-slots structure)))
          (else
           (parse-error "nested: malformed structure" instance)))))

(define (nested-aliased instance names bindings next-instance more-structure)
  ;; example: SLOT ===> (slot-name => alias)

  ;; In this case, SLOT is added to the namespace.  If alias is "_",
  ;; it will be gensymed.
  (let* ((aliased (make-alias next-instance))
         (unbound (gensym))
         (descend-info (bind-descent unbound next-instance instance))
         (ref-info (bind-reference aliased next-instance instance)))
    (descend next-instance
             (parse-structure (cons unbound more-structure)
                              (cons aliased names)
                              (cons ref-info (cons descend-info bindings))))))

(define (nested-unbound instance names bindings next-instance more-structure)
  ;; in this case, generate a symbol for
  ;; the binding and don't add SLOT to the
  ;; namespace                       
  (let* ((unbound (gensym))
         (info (bind-descent unbound next-instance instance)))
    (descend next-instance
             (parse-structure (cons unbound more-structure)
                              names
                              (cons info bindings)))))

;;; grammar
(define structure-name car)
(define structure-slots cdr)

(define identifier? symbol?)

(define (scheme-binding) #f)
(define scheme-binding? not)

(define (aliased? form)
  (and (pair? form)
       (not (null? (cdr form)))
       (eq? (cadr form) '=>)))

(define (slot-name form)
  (cond ((or (identifier? form)
             (scheme-binding? form))
         form)
        ((aliased? form)
         (car form))
        (else
         (parse-error "slot-name: malformed slot" form))))

(define alias caddr)

(define (anonymous-alias? foo)
  (eq? foo '_))

(define (anonymous-aliased? foo)
  (and (aliased? foo)
       (anonymous-alias? (alias foo))))

(define (make-alias slot)
  (if (anonymous-aliased? slot)
      (gensym)
      (alias slot)))

(define (nested? form)
  (and (pair? form)
       (not (aliased? form))))

;;; utility
(define (bind-descent name slot instance)
  (binding name slot instance #t))

(define (bind-reference name slot instance)
  (binding name slot instance #f))

(define (binding name slot instance descended?)
  (list name
        (slot-name slot)
        instance
        (path)
        descended?))

(define (descended-through? binding)
  (list-ref binding 4))

(define-condition type-structure-syntax-error (syntax-error) type-structure-syntax-error?)

(define (parse-error message irritant)
  (type-structure-syntax-error message `(,irritant in ,(pretty-path (path)))))

;; PATH is in reverse order; use left-fold to reverse it and
;; intersperse delimiters.
(define (pretty-path path)
  (if (null? path)
      path
      (fold (lambda (segment acc)
              (cons segment (cons '>> acc)))
            (list (car path))
            (cdr path))))

(define gensym
  (let ((index 0))
    (lambda ()
      (set! index (+ index 1))
      (rename (string->symbol
               (string-append "env-" (number->string index 10)))))))

(define (fold2 kons seed1 seed2 lst)
  (if (null? lst)
      (values seed1 seed2)
      (call-with-values (lambda () (kons (car lst) seed1 seed2))
        (lambda (new-seed1 new-seed2)
          (fold2 kons new-seed1 new-seed2 (cdr lst))))))

;;; Tests
(receive (names bindings)
    (parse-structure '(some-order
                       (bill-to ((street => b_street) street-number street-name apt) state (zip => b_zip))
                       ((ship-to => ship) (street => s_street) (zip => s_zip)))
                     '()
                     '())
  (assert names => '(s_zip s_street ship b_zip state apt street-name street-number b_street)))

(structure->grouped-bindings
 (lambda (sym)
   `(rename ,sym))
 '(foo (bar quux (frob mumble grum)) (baz => _)))

(structure->grouped-bindings
 (lambda (sym)
   `(rename ,sym))
 '((foo => bar) baz))

(structure->grouped-bindings
 (lambda (sym)
   `(rename ,sym))
 '(foo baz))

(receive (names descended-through only-referenced)
    (structure->grouped-bindings
     identity
     '(some-order
       bill-to
       ((ship-to => ship) street state)))

  (assert names => '(bill-to ship street state)))
