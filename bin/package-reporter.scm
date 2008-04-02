(define (ordered-names lst)
  (for-each (lambda (x)
              (and (pair? x)
                   (eq? (car x) 'define-structure)
                   (display
                    (cadr x))
                   (newline)))
            lst))

(define (packages)
  (call-with-input-file
      "packages.scm"
    (lambda (p)
      (ordered-names (read-all read p)))))
