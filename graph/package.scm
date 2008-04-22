;;;; YKK Structures

(define-record-type comp/structure
  (really-make-structure name interface-thunk interface bindings)
  structure?
  (interface-thunk structure-interface-thunk)
  (interface really-structure-interface)
  (bindings structure-bindings)
  (name structure-name))

(define-record-discloser comp/structure
  (lambda (structure)
    `(structure ,(structure-name structure))))

(define (make-structure name interface-thunk bindings)
  (let ((thunk (obj->thunk interface-thunk)))    
    (really-make-structure name
                           thunk
                           (delay (thunk))                           
                           bindings)))

(define (s48->structure struct)
  (make-structure (s48:structure-name struct)
                  (lambda () (s48->interface (s48:structure-interface struct)))
                  struct))

(define (structure-interface structure)
  (force (really-structure-interface structure)))

(define (structure-lookup struct name)
  (if (s48:structure? struct)
      (s48:structure-lookup struct name #f)
      (comp/structure-lookup struct name)))

(define (comp/structure-lookup struct name)
  (receive (found-name info)
      (interface-ref (structure-interface struct) name)
    ;; FIXME: impose-interface
    (and info
         (really-structure-lookup struct found-name))))

(define (really-structure-lookup struct name)
  (let ((bindings (structure-bindings struct)))
    (if (s48:structure? bindings)
        (s48:structure-lookup bindings name #f)
        (lookup-name (structure-bindings struct) name))))

;;;; Utility
(define (obj->thunk foo)
  (if (procedure? foo)
      foo
      (lambda () foo)))

;;;; Tests

(begin

  ;; --------------------
  ;; comp/structure-interface

  (let ((bindings (define-names (make-name-table) `((a . a-value) (b . b-value) (c . c-value))))
        (int (make-simple-interface #f `((a . a-type) (b . b-type)))))

    (let ((struct (make-structure #f int bindings)))
      
      (assert (structure? struct))
      (assert (structure-lookup struct 'a) => 'a-value)
      (assert (structure-lookup struct 'c) => #f))
    
    (let ((struct (s48->structure (s48:get-structure 'scheme))))

      (assert (structure? struct))
      (assert (structure-lookup struct 'car))
      (assert (structure-lookup struct 'foo) => #f))))