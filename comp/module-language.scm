(define-condition
  module-language-error (error)
  module-language-error?)

;;; ; Interface Construction

(define-fluid (*interface-name* #f)
  interface-name
  with-interface-name
  let-interface-name)

(define-syntax define-interface
  (syntax-rules ()
    ((_ name expr)
     (define name (let-interface-name 'name expr)))))

(define-syntax export
  (syntax-rules ()
    ((_ item ...)
     (interface-items (make-interface (interface-name)) (item ...)))))

(define-syntax compound-interface
  (syntax-rules ()
    ((_ interface ...)
     (make-compound-interface (interface-name) (list interface ...)))))

(define-syntax interface-items
  (syntax-rules ()
    ;; public interface
    ((_ apply-to items)
     (interface-items apply-to items ()))

    ;; final recursive step
    ((_ (apply-to ...) () ((name type) ...))
     (apply-to ... (cons 'name type) ...))
    ((_ apply-to () ((name type) ...))
     (apply-to (cons 'name type) ...))    

    ;; case1 ((NAME ...) type)
    ((_ apply-to ((() type) items ...) expanded)
     (interface-items apply-to (items ...) expanded))
    ((_ apply-to (((n1 n2 ...) type) items ...) (expanded ...))
     (interface-items apply-to (((n2 ...) type) items ...) (expanded ... (n1 type))))    

    ;; case2 (NAME TYPE)
    ((_ apply-to ((name type) items ...) (expanded ...))
     (interface-items apply-to (items ...) (expanded ... (name type))))

    ;; case3 NAME
    ((_ apply-to (name items ...) (expanded ...))
     (interface-items apply-to (items ...) (expanded ... (name undeclared-type))))))

;;;; Interface Manipulation

(define-syntax subset
  (syntax-rules ()
    ((_ int (name ...))
     (modify int (expose name ...)))))

(define-syntax with-prefix
  (syntax-rules ()
    ((_ int name)
     (modify int (prefix name)))))

(define-syntax modify
  (syntax-rules ()
    ((_ int modifier)
     (modifier->procedure int modifier))
    ((_ int m1 m2 ...)
     ;; modifiers are nested in right-to-left order
     (modify (modify int m2 ...) m1))))

(define (prefix int sym)
  (fold->new (lambda (name info acc)
               (modify-item (concatenate-symbol sym name)
                            name
                            info
                            acc))             
             int))

(define (expose int . names)
  (fold->new (lambda (name acc)
               (receive (found-name info)
                   (interface-ref int name)
                 (if info
                     (add-item found-name info acc)
                     acc)))
             names))

(define (hide int . names)
  (fold->new (lambda (name info acc)
               (if (memq name names)
                   acc
                   (add-item name info acc)))
             int))

(define (alias int . from/to-pairs)
  (fold->new (lambda (name info acc)
               (add-item name info
                         (or (maybe-rename from/to-pairs name info acc)
                             acc)))             
             int))

(define (rename int . from/to-pairs)
  (fold->new (lambda (name info acc)
               (or (maybe-rename from/to-pairs name info acc)
                   (add-item name info acc)))
             int))

;; internal, here for clarity
(define (maybe-rename from/to-pairs name info acc)
  (cond ((assq name from/to-pairs) =>
         (lambda (pair) (modify-item (cdr pair) name info acc)))
        (else #f)))

(define (fold->new proc obj)
  (cond ((or (pair? obj) (interface? obj)) (fold->new-interface proc obj))
        ((structure? obj) (fold->new-structure proc obj))        
        ((s48:interface? obj) (fold->new-interface proc (s48->interface obj)))        
        ((s48:structure? obj) (fold->new-structure proc (s48->structure obj)))
        (else (module-language-error "fold->new: unexpected fold object" obj))))

(define add-item alist-cons)

(define (modify-item new-name old-name old-info acc)
  (if (original-item? old-info)
      (cons (re-modify new-name old-info) acc)      
      (cons (modified-item new-name old-name old-info) acc)))

;; --------------------
;; Internal

;; A "modified name" is just some original entry in an interface
;; shadowed by another name.  Model it by preserving the information
;; in the original and CONSing the new name on the front.

;; INTERFACE-REF is interested in dereferencing the new name and
;; returning the original interface entry.  INTERFACE-FOLD is
;; interested in allowing full access to the modification so it can be
;; re-shadowed by a subsequent modification.

(define-interface-maker (make-modified-interface)
  (ref original-item-name original-item-info)
  (fold item-name item-info))

;; An "original item" looks like: (NAME . TYPE)
;; A "modified item" looks like: (NAME . (REAL-NAME . TYPE))

(define (modified-item new old info)
  (cons new (cons old info)))

(define re-modify cons)

(define (modified-name? foo)
  (and (pair? foo) (original-item? (cdr foo))))

(define item-name car)
(define item-info cdr)

(define original-item? pair?)

(define (original-item-name foo)
  (if (modified-name? foo)
      (cadr foo)
      (item-name foo)))

(define (original-item-info foo)
  (if (modified-name? foo)
      (cddr foo)
      (cdr foo)))

(define-syntax modifier->procedure
  (syntax-rules ()
    ((_ int (proc (from to) ...))
     (proc int '(from . to) ...))
    ((_ int (proc args ...))
     (proc int 'args ...))))

(define (fold->new-structure proc structure)
  (make-structure (structure-name structure)
                  (lambda () (fold->new-interface proc (structure-interface structure)))
                  (structure-bindings structure)))

(define (fold->new-interface proc interface)  
  (make-modified-interface
   #f
   (if (interface? interface)       
       (fold-declarations proc '() interface)
       (fold-right proc '() interface))))

;;;; Tests

(begin

  ;; --------------------
  ;; comp/module-language-interface

  (let* ((cons-item (lambda (k v l) (cons (cons k (if (original-item? v) (original-item-info v) v)) l)))         
         (interface->list (lambda (int) (fold-declarations cons-item '() int)))
         (equiv (lambda (int) (assert (interface->list int) => `((cat . a-type) (carb . b-type))))))    

    ;; --------------------
    ;; interface modifiers
    
    (equiv (export (cat 'a-type) (carb 'b-type)))
    (equiv (compound-interface (export (cat 'a-type)) (export (carb 'b-type))))
    (equiv (subset (export (cat 'a-type) ((carb c d) 'b-type)) (cat carb)))
    (equiv (with-prefix (export (at 'a-type) (arb 'b-type)) c))
    (equiv (modify (export ((cat bat trap) 'a-type) (carl 'b-type))
                   (hide trap)
                   (rename (carl carb))
                   (alias (trap cat))
                   (hide cat bat)))

    ;; --------------------
    ;; structure modifiers

    (let* ((bindings (define-names (make-name-table) `((cat . a-value) (carb . b-value) (crust . c-value))))
           (struct (make-structure #f (export cat carb crust) bindings))
           (mod1 (with-prefix struct p/))
           (mod2 (modify mod1 (hide p/carb) (rename (p/cat p/dog)))))
      (assert (structure-lookup mod1 'p/cat) => 'a-value)
      (assert (structure-lookup mod1 'cat) => #f)
      (assert (structure-lookup mod2 'p/dog) => 'a-value)
      (assert (structure-lookup mod2 'p/carb) => #f)
      (assert (structure-lookup mod2 'p/crust) => 'c-value)
      (assert (structure-lookup mod1 'crust) => #f))

    ;; --------------------
    ;; scheme48 modifiers

    (let* ((orig (s48:get-structure 'scheme))
           (mod1 (with-prefix orig p/)))
      (assert (structure-lookup mod1 'p/car))
      (assert (not (structure-lookup mod1 'car))))))

(begin
  ;; --------------------
  ;; Internal

  (assert (interface-items list (foo (bar ':symbol) ((baz quux) ':number))) =>
          `((foo . ,undeclared-type) (bar . :symbol) (baz . :number) (quux . :number))))
