;;;; Grammar
(define (self-evaluating? item)
  (or (boolean? item)
      (char? item)
      (string? item)
      (number? item)))

(define (quotation? item)
  (and (pair? item)
       (eq? 'quote (car item))))

(define (literal? item)
  (or (self-evaluating? item)
      (quotation? item)))

(define (macro-use? item . env)
  (let ((env (optional-env env)))    
    (cond ((and (pair? item)
                (package-lookup env (car item)))
           => (lambda (binding)
                (eq? :syntax (binding-type binding)))))))

(define (procedure-call? item . env)
  (and (pair? item)
       (not (macro-use? item (optional-env env)))))

(define (keyword? name)
  (and (symbol? name)       
       (let ((s (symbol->string name)))    
         (char=? #\: (string-ref s (- (string-length s) 1))))))

;;;; Utility
(define (quote-non-literal foo)
  (if (literal? foo)
      foo
      `',foo))

(define (remove-keyword-indication name)
  (let* ((s (symbol->string name))
         (size (string-length s)))
    (if (char=? #\: (string-ref s (- size 1)))
        (string->symbol (substring s 0 (- size 1)))
        name)))

;;;; Syntax Continuation Primitives
(define (continue value k)
  `(,(car k) ,value ,@(cdr k)))

(define (continue/values values k)
  `(,(car k) ,@values ,@(cdr k)))

(define (continue-into value k)
  `(,@k ,value))

(define (continue-into/values values k)
  `(,@k ,@values))

;;;; Syntax Definition

;; see bcomp/cenv.scm and bcomp/package.scm

(define (define-now! name form . env)
  (let ((env (optional-env env)))    
    (environment-define! env name (eval form env))))

(define (force-up! for-syntax-name binding-name)
  (let ((value (environment-ref (interaction-environment) binding-name)))
    (environment-define! (for-syntax-environment) for-syntax-name value)))

(define (up-one-tower-level env)
  (cdr (force (environment-macro-eval (package->environment env)))))

(define (for-syntax-environment)
  (up-one-tower-level (interaction-environment)))

(define (definition-value name)
  (environment-ref (interaction-environment) name))

;;;; Macro Expansion

;; see env/debug.scm and bcomp/syntax.scm

(define (expand form . env)
  (let ((cenv (package->environment (optional-env env))))
    (really-expand form env)))

(define (map-expand forms . env)
  (map-in-order
   (cute really-expand <> (package->environment (optional-env env)))
   forms))

(define (really-expand form cenv)
  (schemify (expand-form form cenv)))

(define (apply-macro-transformer transformer form rename compare)
  (let* ((transform (transformer-procedure transformer))
         (applied `(,(car form) ,@(map-expand (cdr form))))
         (result (transform applied rename compare)))    
    (if (eq? result applied)        
        (syntax-error "use of macro doesn't match definition"
                      (cons (schemify (car form))
                            (desyntaxify (cdr form))))
        result)))

(define (transformer-procedure foo)
  (cond ((and (pair? foo)
              (procedure? (car foo)))
         (car foo))
        ((procedure? foo)
         foo)
        (else
         (error 'transform-procedure
                foo))))

;;;; Syntax SRFI-89 Support
(define (error/syntax . args)
  (apply error (map-in-order desyntaxify args)))

(define (srfi-89:require-positionals name A V n)
  (if (>= (length A) n)
      (let ((taken rest (split-at A n)))
        (values rest (append V taken)))      
      (error/syntax 'too-few-positional-parameters
                    name
                    `(required: ,n)
                    `(given: ,(length A)))))

(define (srfi-89:optional-positionals name A V options)
  (let loop ((A A) (options options) (acc '()))
    (if (or (null? A) (null? options))
        (values A (append V (reverse acc) options))
        (loop (cdr A)
              (cdr options)
              (cons (car A) acc)))))

(define (srfi-89:named-parameters name A V named)
  (let* ((alist remainder (keywords->alist A #f))
         (alist (map-car desyntaxify alist))
         (allowed (map car named))
         (bad (remove (lambda (pair) (memq (car pair) allowed))
                      alist)))
    (if (not (null? bad))
        (error/syntax 'unexpected-keyword name `(unexpected: ,@bad))
        (values
         remainder
         (append V (project-alist->named name alist named '() '()))))))

(define (keywords->alist source degenerate?)
  (let loop ((s source) (acc '()))
    (cond ((or (null? s)
               (not (keyword? (car s))))           
           (values acc s))
          ((or (null? (cdr s))
               (keyword? (cadr s)))
           (if degenerate?
               (loop (cdr s)
                     (cons (list (remove-keyword-indication (car s)) #t)
                           acc))
               (error 'take-keywords->alist
                      "degenerate source list"
                      (car s))))
          (else
           (loop (cddr s)
                 (cons (list (remove-keyword-indication (car s)) (cadr s))
                       acc))))))

(define (project-alist->named name alist named found acc)
  (let loop ((named named) (found found) (acc acc))    
    (if (null? named)
        (reverse acc)
        (let ((key (caar named)))
          (cond
           ((memq key found)
            (error/syntax 'duplicate-keyword-argument name key))
           ((assq key alist)
            => (lambda (found)
                 (loop (cdr named)
                       (cons key found)
                       (cons (cadr found) acc))))
           ((null? (cdar named))
            (error/syntax 'missing-required-keyword name key))
           (else
            (loop (cdr named)
                  (cons key found)
                  (cons (cadar named) acc))))))))

(define (srfi-89:parse-formals name formals)
  (let loop ((f formals) (stack '()) (t '()) (allow-positional? #t) (allow-named? #t))
    (cond ((rest? f)
           (if (symbol? f)                   
               (values (reverse (cons f t)) (stack-rest stack))
               (parse-error name f "strange-looking rest formal")))
          ((null? f)
           (values (reverse t) (stack-no-rest stack)))              
          ((positional? (car f))
           (if allow-positional?
               (let ((f required options (take-positionals name f)))
                 (loop f
                       (stack-options options (stack-required required stack))
                       (append (map-in-order car options) required t)
                       #f
                       allow-named?))                   
               (parse-error name f "only one positional section allowed")))              
          ((named? (car f))
           (if allow-named?
               (let ((f named (take-named name f)))
                 (loop f
                       (stack-named named stack)
                       (append (map-in-order cadr named) t)
                       allow-positional?
                       #f))
               (parse-error name f "only one keyword section allowed")))          
          (else
           (error 'malformed-formals)))))

(define (take-positionals name f)
  (let loop ((f f) (required '()) (options '()))        
    (cond ((or (rest? f) (null? f) (not (positional? (car f))))
           (values f required options))
          ((optional? (car f))
           (if (null? (cdar f))
               (parse-error name f "optional positional parameters require defaults")
               (loop (cdr f)
                     required
                     (cons (car f) options))))
          ((symbol? (car f))
           (if (not (null? options))
               (parse-error name f "cannot add required positional parameters after optionals")
               (loop (cdr f)
                     (cons (car f) required)
                     options)))
          (else
           (parse-error name f "unrecognized positional parameter format")))))

(define (take-named name f)
  (let loop ((f f) (found '()))
    (cond ((or (rest? f) (null? f) (not (named? (car f))))
           (values f found))
          ((null? (cdar f))
           (parse-error name f "malformed keyword formal"))
          (else
           (loop (cdr f)
                 (cons (car f) found))))))

(define (stack-required r stack)
  (if (null? r)
      stack
      (cons `(positional: ,(length r))
            stack)))

(define (stack-options o stack)
  (if (null? o)
      stack
      ;; REVERSE here because they were accumulated tail-recursively
      (cons `(optional: ,(map cadr (reverse o)))
            stack)))

(define (stack-named n stack)
  (if (null? n)
      stack
      ;; implicit REVERSE in FOLD because N was accumulated tail-recursively
      (cons `(named: ,(fold (lambda (pair acc)
                              (let ((label (remove-keyword-indication (car pair))))
                                (cons (if (null? (cddr pair))
                                          (list label)
                                          (list label (caddr pair)))
                                      acc)))
                            '()
                            n))
            stack)))

(define (stack-rest stack)
  (cons '(rest:) stack))

(define (stack-no-rest stack)
  (cons '(no-rest:) stack))

(define (parse-error name formals message)
  (error/syntax 'malformed-formals message name formals))

(define (optional? foo)
  (and (pair? foo)
       (not (named? foo))))

(define (positional? foo)
  (or (symbol? foo)
      (optional? foo)))

(define (named? foo)
  (and (pair? foo)
       (keyword? (car foo))))

(define (rest? formals)
  (and (not (pair? formals))
       (not (null? formals))))

(define (srfi-89:stack->k allowed seed stack)
  
  (define (match key)
    (cond ((assq (desyntaxify key) allowed) => cadr)
          (else (error 'unrecognized-stack-keyword
                       (desyntaxify key)))))

  (fold (lambda (s k)
          `(,(match (car s)) ,@(cdr s) ,k))
        seed
        stack))

(define (beta-substitute lookup names values)
  (let loop ((E '()) (T names) (V values))
    (if (null? V)
        (reverse (map-in-order cdr E))
        (let* ((name (car T))
               (value (car V))
               (new-value (map-tree (cut substitute lookup <> E) value)))
          (loop (alist-cons name new-value E)
                (cdr T)
                (cdr V))))))

(define (substitute lookup value E)
  (cond ((lookup value E) => cdr)
        (else value)))

(define (map-tree proc tree)
  (let loop ((tree tree))          
    (cond ((null? tree)
           tree)
          ((not (pair? tree))
           (proc tree))          
          ((and (pair? tree)
                (not (null? (cdr tree))))
           (cons (loop (car tree))
                 (loop (cdr tree))))
          (else
           (map-in-order loop tree)))))

;;;; Internal
(define (optional-env option)
  (if (null? option)
      (interaction-environment)
      (car option)))

(begin
  (assert (quote-non-literal 1) => 1)
  (assert (quote-non-literal 'foo) => ''foo)  
  (assert (quote-non-literal ''foo) => ''foo)

  (assert (remove-keyword-indication 'foo) => 'foo)
  (assert (remove-keyword-indication 'foo:) => 'foo)
  (assert (remove-keyword-indication 'foo::) => 'foo:)

  (let ((alist rest (keywords->alist '(foo: 1 bar: 2 baz) #f)))
    (assert alist => '((bar 2) (foo 1)))
    (assert rest => '(baz)))

  (let ((alist rest (keywords->alist '(foo: 1 bar: baz:) #t)))
    (assert alist => '((baz #t) (bar #t) (foo 1)))
    (assert rest => '()))

  (let ((T stack (srfi-89:parse-formals 'foo '(a (b 1) (c: c 2) (d: d) . r))))
    (assert T => '(a b c d r))
    (assert stack => '((rest:) (named: ((c 2) (d))) (optional: (1)) (positional: 1))))

  (assert
   (srfi-89:stack->k
    `((rest: rest) (named: named) (optional: optional) (positional: positional))
    '(seed)
    '((rest:) (named: ((c 2) (d))) (optional: (1)) (positional: 1)))
   => '(positional 1 (optional (1) (named ((c 2) (d)) (rest (seed))))))

  (assert
   (beta-substitute assq '(a b c) '((+ d 1) (+ a 1) 3))
   => '((+ d 1) (+ (+ d 1) 1) 3))  
  )
