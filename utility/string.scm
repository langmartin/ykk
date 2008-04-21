(define (string->normal-symbol str)
  (string->symbol (string-downcase str)))

(define (string->label s)
  (normalize-string char-set:letter+digit " " string-titlecase! s))

(define (string->name s)
  (normalize-string char-set:letter+digit "_" string-downcase! s))

(define (string->identifier s)
  (normalize-string char-set:letter+digit "-" string-downcase! s))

(define tech-name string->identifier)

(define (normalize-string char-set delimitor transform! s)
  (let* ((tokens (string-tokenize s char-set))
         (normal (string-join tokens delimitor)))
    (transform! normal)
    normal))
