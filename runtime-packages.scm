(define-structure rss-reader-milestone
  (export)
  (open scheme+
        threads
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
        alists)
  (files rss-reader-milestone))
