(define-structure specific-generics
  (export html
          csv)
  (open scheme
        ykk/methods
        assert))

(define-structure site-zero
  (export foo)
  (open scheme
        htmlprag
        http
        assert
        url
        ykk-parsing
        srfi-13
        srfi-1)
  (files site-zero))

(define-interface date-parsing-interface
  (export
   strftime
   date-parse
   date-format))

(define-structure date-parsing
  date-parsing-interface
  (open scheme
        posix
        language-ext
        assert)
  (files (utility date-fork)))

;; (define-structure file-uploader
;;   (export foo)
;;   (open scheme
;;         htmlprag
;;         http
;;         assert)
;;   (files file-uploader.scm))
