(define-structure site-zero
  (export foo)
  (open scheme
        htmlprag
        http-client
        assert)
  (files site-zero.scm))

(define-structure file-uploader
  (export foo)
  (open scheme
        htmlprag
        http-client
        assert)
  (files file-uploader.scm))
