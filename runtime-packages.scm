(define-structure rss-reader-milestone
  (export)
  (open extra-scheme
        srfi-1+
        ykk/records
        (with-prefix persisted-graph source:)
        scanned-graph        
        assert
        methods
        primitive-types
        persistent-immutable
        proc-def
        http
        ykk/record-procedural
        description
        forms
        srfi-13
        alists)
  (files rss-reader-milestone))
