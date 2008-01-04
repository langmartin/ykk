;;;; A subset of the module configuration language defined over ykk/environments.

;;;; See http://mumble.net/~campbell/s48-refman/html/Module-configuration-language.html

;;;; Public
(define-syntax subset
  (syntax-rules ()
    ((_ env (name ...))
     (modify env (expose name ...)))))

(define-syntax with-prefix
  (syntax-rules ()
    ((_ env name)
     (modify env (prefix name)))))

(define-syntax modify
  (syntax-rules ()
    ((_ env modifier)
     (modifier->procedure env modifier))
    ((_ env m1 m2 ...)
     (modify (modify env m2 ...) m1))))

(define (prefix env sym)
  (fold-names (lambda (name binding new-env)
                (define-name new-env
                  (concatenate-symbol sym name)
                  binding))
              (make-name-table)
              env))

(define (expose env . names)
  (fold (lambda (name new-env)
          (define-name new-env
            name
            (lookup env name)))
        (make-name-table)
        env))

(define (hide env . names)
  (delete-names env names))

(define (alias env . from/to-pairs)
  (fold (lambda (pair new-env)
          (define-name new-env
            (cdr pair)
            (lookup env (car pair))))
        env
        from/to-pairs))

(define (rename env . from/to-pairs)
  (fold (lambda (pair new-env)
          (define-name (remove-name new-env (car pair))
            (cdr pair)
            (lookup (car pair))))
        env
        from/to-pairs))

;;;; Internal
(define-syntax modifier->procedure
  (syntax-rules (alias rename)
    ((_ env (name (from to) ...))
     (name env '(from . to) ...))
    ((_ env (name args ...))
     (name env 'args ...))))