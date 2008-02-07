(templates
 (template
  page
  (lambda (title content)
    `(html
      (head (title ,title))
      (body
       ,content))))
 (template
  s-exp
  (lambda (expr)
    expr)))



(types
 (type
  news-item
  (list a b)))

(template page (data page news))






(data
 (datum
  (pages ((foo 1) (bar 2)))
  (datum
   (news '())
   (datum
    (list '())
    (news-item foo bar)
    (news-item 3 4)
    (news-item 5 6)))))


(templates
 (template
  (pages page)
  `(html (head (title "go"))
         (body
          ,page))

  (template
   (news news))
  
  
  ))
