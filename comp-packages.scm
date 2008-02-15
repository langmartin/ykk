(define-structure comp/s48-dependencies
  (export undeclared-type

          s48:interface?
          s48:interface-ref
          s48:for-each-declaration

          s48:structure-name
          s48:structure-interface
          s48:structure?
          s48:structure-lookup
          
          s48:get-structure
          s48:get-interface

          ;; for tests
          s48:make-simple-interface)
  
  
  (open (subset meta-types (undeclared-type))
        (with-prefix interfaces s48:)
        (with-prefix packages s48:)
        (with-prefix packages-internal s48:)

        ;; for procedures below
        scheme
        environments
        package-commands-internal)

  (begin
    (define s48:get-structure get-structure)
    (define (s48:get-interface name)
      (environment-ref (config-package) name))))


(define-structures ((comp/interfaces comp/interfaces-interface)
                    (comp/interface-creator comp/interface-creator-interface))  
  (open scheme
        srfi-1 srfi-8 srfi-9+
        types
        ykk/types type-reflection
        assert
        fluids+
        conditions+        
        comp/s48-dependencies)
  
  (files (comp interface)))

(define-structures ((comp/structure comp/structure-interface)
                    (comp/structure-reflection comp/structure-reflection-interface))
  (open scheme
        srfi-8 srfi-9+
        assert
        ykk/names ; FIXME: make this comp/names
        comp/interfaces comp/interface-creator
        comp/s48-dependencies)
  (files (comp package)))

(define-structures ((comp/module-language comp/module-language-interface)
                    (comp/module-language-create comp/module-language-creator-interface))
  (open scheme
        srfi-1 srfi-8
        assert
        fluids+
        conditions+
        big-util ; concatenate-symbol
        ykk/names ; for testing
        comp/interfaces comp/interface-creator
        comp/structure comp/structure-reflection
        comp/s48-dependencies)
  (files (comp module-language)))
