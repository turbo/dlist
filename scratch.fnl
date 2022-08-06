
(local inspect (require :inspect))
(local dlist (require :dlist))


(let [l (dlist)]
  (print l.length)
  (l.push-front 42)
  (print l.length)
  (l.init)
  (print l.length)
  (l.push-back :two)
  (l.push-back :three)
  (l.push-front :first)
  (print l.length)
  (each [e (l.elements)]
    (print (inspect e.value)))
  (print)
  (each [e (l.elements-reverse)]
    (print (inspect e.value)))
  (print)
  (print (inspect (l.as-table))))

(let [l (dlist [ 1 2 { :foo :bar } ])]
  (each [e (l.elements)]
    (print (inspect e.value)))
  (print (inspect (. (. l 2) :value))))

(let [l (dlist [ 1 2 3 ])
      m (dlist [ 4 5 6 ])]
  (m.push-front-list l)
  (print (inspect (m.as-table)))
  (m.push-back-list l)
  (print (inspect (m.as-table)))
  )

(print)
(print)

; index access optimization
(let [l (dlist [ 1 2 3 4 5 ])]
  (assert (= l.length 5))
  (print (inspect (l.as-table)))
  (for [n 1 5]
    (local iv (. l n))
    (print (.. n " -> " iv.value))
    (assert (= iv.value n)
            (.. n "!=" iv.value)))

  (let [te (. l 2)]
    (l.insert-before 1.5 te)
    (assert (l.has-value 1.5))
    (assert (not (l.has-value 15)))
    (assert (l.has-element te))
    (print (inspect (l.as-table)))))

(print)
(print)

(let [l (dlist [ 1 2 3 3 3 4 4])]
  (print (inspect (l.as-table)))
  (l.remove-value 3)
  (l.remove-value 4)
  (l.remove-value 6)
  (print (inspect (l.as-table)))
  (l.remove-value-all 3)
  (l.remove-value-all 4)
  (print (inspect (l.as-table))))
