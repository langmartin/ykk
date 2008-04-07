;;;; "Type Tree"
(define-record-type/primitive :folder
  nongenerative: ua8db299f-732b-4947-ba86-074e4a384bba
  (sticky (type :boolean)))

(define-record-type/primitive :person
  nongenerative: u2a019585-f45a-4ea9-9001-ee1186d9df51
  (name   (type :string))
  (age    (type :number))
  (gender (type :symbol)))

(define-record-type/primitive :place
  nongenerative: ufb76a3cd-903c-413f-86a0-3f36e74513c2
  (address (type :string))
  (city    (type :string))
  (state   (type :symbol))
  (zip     (type :string)))

(define-record-type/primitive :thing
  nongenerative: u5ac8fe2a-0a7a-475f-a336-d3441cc1517d
  (color   (type :string))
  (weight  (type :number))
  (texture (type :symbol)))

;;;; Top
(define (persist-once name thunk)
  (if (not (persistent-symbol name))
      (persistent-symbol-set! name (thunk))))

(define (top)
  (persistent-symbol 'top))

(define (scanned-top)
  (scan (top)))

(define (update-scanned-top new-top)
  (persistent-symbol-set!
   'top
   (scanned->source new-top)))

(persist-once
 'top
 (lambda ()
   (source:root (source:node :folder
                             (plist (sticky #f))))))

;;;; Manipulation
(define (zip-up/update-top z)
  (update-scanned-top
   (zip-all-the-way-up z)))

(define (perform z op)
  (zip-up/update-top
   (move 'up (op (z-item z)) z)))

(define (perform/path p op)
  (perform (resolve (scanned-top) p) op))

(define (insert g child)
  (replace-children
   g
   (add-child child (graph-children g))))

;; (perform/path
;;  "/"
;;  (cut insert <>
;;       (edge 'first-person
;;             (node :person (plist (name "Lang")
;;                                  (age 31)
;;                                  (gender 'male))))))

;; (map->list graph-name (scanned-top))

;; (graph-name (z-item (resolve (scanned-top) "/first-person")))


