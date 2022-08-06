Doubly linked list that fully implements the same interface as Go's `container/list` type. Extra functions for init-from-table and direct index access are provided.

Usage:

```fennel
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
  
; etc, pp.
```
