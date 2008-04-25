(define (string->normal-symbol str)
  (string->symbol (string-downcase str)))

;; A label is a string with spaces between the words and each word
;; is capitalized.
(define (string->label s)
  (normalize-string char-set:letter+digit " " string-titlecase! s))

;; A name is a string with underscores between the words and each word
;; is lowercase.  This is useful for form names.
(define (string->name s)
  (normalize-string char-set:letter+digit "_" string-downcase! s))

;; An identifier is a string with hypens between the words and each
;; word is lowercase.
(define (string->identifier s)
  (normalize-string char-set:letter+digit "-" string-downcase! s))

(define tech-name string->identifier)

(define (camel-case s)
  (let ((tokens (string-tokenize s char-set:letter+digit)))    
    (apply string-append
           (string-downcase (car tokens))
           (map string-titlecase (cdr tokens)))))

(define (normalize-string char-set delimitor transform! s)
  (let* ((tokens (string-tokenize s char-set))
         (normal (string-join tokens delimitor)))
    (transform! normal)
    normal))

(define (maybe-tokenize s . rest)
  (cond ((string? s)
         (apply string-tokenize s rest))
        ((pair? s)
         s)
        (else
         (error 'wrong-type-argument
                "maybe-tokenize: expecting string or list"
                s))))

;;;; Tests
(begin
  (assert (string->normal-symbol "FOO") => 'foo)
  (assert (string->label "foo bar") => "Foo Bar")
  (assert (string->name "foo bar") => "foo_bar")
  (assert (string->identifier "foo bar") => "foo-bar")
  (assert (camel-case "foo_bar") => "fooBar")
  (assert (maybe-tokenize "foo bar") => '("foo" "bar"))
  (assert (maybe-tokenize '("foo" "bar")) => '("foo" "bar")))