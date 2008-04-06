(define (maybe-symbol? foo)
  (or (not foo)
      (symbol? foo)))

(define-simple-type :maybe-symbol (:symbol :boolean) maybe-symbol?)