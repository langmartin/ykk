(letrec ((html-4.0-strict "strict"))

  (define pages
    (letrec ((site-title "foo")
             (site-url "http://foo/"))
      `(html (@ (doctype html-4.0-strict))
             (head
              (title site-title)
              (link (@ (type "text/css") (rel "stylesheet"))))
             (body
              ,(define news
                 (letrec ((section "news"))
                   `(div (@ (class "news"))
                         (ul
                          ,(map (lambda (x)
                                  `(li ,@x))
                                (take (path news-articles)
                                      2))))))

              ,(define old
                 (letrec ((section "news"))
                   `(div (@ (class "news"))
                         (ul
                          ,(map (lambda (x)
                                  `(li ,@x))
                                (drop (path news-articles)
                                      2))))))

              ,(define newsest
                 (letrec ((foo "bar"))
                   `(div (@ (class "foo"))
                         (p "first one")
                         (p "second one")
                         (p "third one"))))
              
              ))))
  
  (define news-articles
    (list
     (list "title" "some" "things")
     (list "title" "some" "things2")
     (list "title" "some" "things3")
     (list "title" "some" "things4")))

  ;; (define better-typed
;;     (letrec ((thing (make-a-type a b)))
;;       (define thing-list
;;         (list
;;          (thing 3 4)
;;          (thing 5 6)))
;;       (define indexed-things
;;         (r/b-tree
;;          (thing 7 8)
;;          (thing 8 9)))))
  )

