(define-structure specific-generics
  (export html
          csv)
  (open scheme
        generics
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



;; (define-structure file-uploader
;;   (export foo)
;;   (open scheme
;;         htmlprag
;;         http
;;         assert)
;;   (files file-uploader.scm))
