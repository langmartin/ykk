(define-condition
  interface-error (error)
  interface-error?)

;;;; YKK interfaces
(define-record-type comp/interface
  (really-make-interface ref fold simple type)
  interface?
  (ref ref-method)
  (fold fold-method)  
  (simple simple-name)
  (type interface-type))

(define (simple-interface? interface)
  (and (simple-name interface)       
       (not (interface-type interface))))

(define (type-interface? interface)
  (ykk-type? (interface-type interface)))

(define (compound-interface? interface)
  (let ((type (interface-type interface)))
    (and type (pair? type))))

(define (s48-interface? interface)
  (s48:interface? (interface-type interface)))

(define-record-discloser comp/interface
  (lambda (obj)
    `(interface ,(or (simple-name obj)
                     (interface-type obj)))))

;; universal constructor
(define (make-interface . rest)
  (cond ((null? rest)
         (interface-error "make-interface: no interfaces were given"))
        ((ykk-type? (car rest))
         (type->interface (car rest)))
        ((s48:interface? (car rest))
         (s48->interface (car rest)))        
        ((and (pair? (cdr rest)) (not (pair? (cadr rest))))         
         (make-compound-interface (car rest) (cdr rest)))        
        (else
         (make-simple-interface (car rest) (cdr rest)))))

(define (fold-declarations proc seed int)
  ((fold-method int) proc seed))

;;;; Scheme 48 compatibility

(define (interface-ref int name)
  ((ref-method int) name))

(define (interface-member? int name)
  (receive (found type)
      (interface-ref int name)
    (and found #t)))

(define (for-each-declaration proc int)
  (fold-declarations (lambda (name type acc)
                       ;; NAME NAME is not a typo, see s48/bcomp/interface.scm
                       (proc name name type)
                       acc)
                     ;; unspecific
                     (if #f #f)
                     int))

;;;; Interfaces defined over ykk types

(define (type->interface type)
  (really-make-interface (slot-ref-by-name type)
                         (slot-fold type)
                         #f
                         type))

(define (slot-ref-by-name type)
  (lambda (name)
    (let ((slot (type-slot-ref type name)))
      (if slot
          (name-found (slot-name slot)
                      (slot-type slot))
          (name-not-found)))))

(define name-found values)

(define (name-not-found)
  (values #f #f))

(define (slot-fold type)
  (lambda (proc seed)
    (fold-right (lambda (slot acc)
                  (proc (slot-name slot)
                        (slot-type slot)
                        acc))                
                seed
                (type-slots type))))

;;;; Simple Interfaces 
(define-syntax define-interface-maker
  (syntax-rules (ref fold)
    ((_ (procedure-name select-name select-info))
     (define-interface-maker (procedure-name)
       (ref select-name select-info)
       (fold select-name select-info)))     
    ((_ (procedure-name) (ref ref-name ref-info) (fold fold-name fold-info))     
     (define (procedure-name name items)
       (really-make-interface (simple-item-ref items ref-name ref-info)
                              (simple-item-fold items fold-name fold-info)
                              name
                              #f)))))

(define (simple-item-ref items return-name info)
  (lambda (name)
    (cond ((assq name items) =>
           (lambda (item) (name-found (return-name item) (info item))))
          (else (name-not-found)))))

(define (simple-item-fold items return-name info)
  (lambda (proc seed)
    (fold-right (lambda (item acc)
                  (proc (return-name item) (info item) acc))
                seed
                items)))

(define-interface-maker (make-simple-interface car cdr))

;;;; Compound Interfaces (similar to s48 compound interfaces, but operate over simple and type interfaces)

(define (make-compound-interface name interfaces)
  (really-make-interface (compound-ref-by-name interfaces)
                         (compound-fold interfaces)
                         name
                         interfaces))

(define (compound-ref-by-name interfaces)
  (lambda (name)    
    (or (fold/return (lambda (return int acc)
                       (receive (name type)
                           ((ref-method int) name)
                         (and name (return (name-found name type)))))
                     #f
                     interfaces)
        (name-not-found))))

(define (compound-fold interfaces)
  (lambda (proc seed)
    (fold-right (lambda (int seed)
                  ((fold-method int) proc seed))
                seed
                interfaces)))

;;;; S48 interface wrapper

(define (s48->interface s48-int)
  (really-make-interface (s48-ref s48-int)
                         (s48-fold s48-int)
                         #f
                         s48-int))

(define (s48-ref int)
  (lambda (name)    
    (s48:interface-ref int name)))

(define (s48-fold int)
  (lambda (proc seed)
    (let ((acc seed))
      (s48:for-each-declaration (lambda (ignore name type)
                                  (set! acc (proc name type acc)))
                                int)
      acc)))

;;;; Utility

(define-syntax fold/return
  (syntax-rules ()
    ((_ (lambda (return formals ...) body ...) seed list)
     (call-with-current-continuation
      (lambda (return)
        (fold (lambda (formals ...) body ...)
              seed
              list))))
    ((_ return proc seed list)
     (fold/return (lambda (return item acc) (proc return item acc))
                  seed list))))

;;;; Tests
(begin
  
  ;; --------------------
  ;; comp/interfaces-interface
  
  (let* ((some-type (type-definition () (a 'a-type) (b 'b-type)))
         (interface->list (lambda (int) (fold-declarations alist-cons '() int)))         
         (equiv (lambda (int) (assert (interface->list int) => `((a . a-type) (b . b-type))))))

    ;; --------------------
    ;; construction

    ;; interface over ykk type
    (equiv (make-interface some-type))
    (equiv (type->interface some-type))

    ;; simple interface
    (equiv (make-interface #f (cons 'a 'a-type) (cons 'b 'b-type)))
    (equiv (make-simple-interface #f `((a . a-type) (b . b-type))))

    ;; compound interface
    (let ((a (make-simple-interface #f `((a . a-type))))
          (b (make-simple-interface #f `((b . b-type)))))
      (equiv (make-interface #f a b))
      (equiv (make-compound-interface #f (list a b))))

    ;; s48 interface
    (let ((s48-int (s48:make-simple-interface #f `((a a-type) (b b-type)))))
      (equiv (make-interface s48-int))
      (equiv (s48->interface s48-int)))

    ;; --------------------
    ;; reflection
    
    (let* ((def `((a . a-type) (b . b-type)))
           (int (make-simple-interface #f def)))

      (assert (interface? int))
      
      (receive (name type) (interface-ref int 'a)
        (assert name => 'a)
        (assert type => 'a-type))

      (receive (name type) (interface-ref int 'c)
        (assert (not (or name type))))

      (assert (interface-member? int 'a))
      (assert (not (interface-member? int 'c)))

      (let ((acc '()))
        (for-each-declaration (lambda (name name type)
                                (set! acc (alist-cons name type acc)))
                              int)
        (assert acc => (fold-declarations alist-cons '() int))))))