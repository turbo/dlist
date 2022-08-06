(local inspect (require :inspect))

(λ [?src-table]
  (let [self { :length 0
               :__refmap { } 
               }]

    (λ self.init []
      (doto self
        (tset :length 0)
        (tset :__refmap { })
        (tset :front nil)
        (tset :back nil)))

    (λ self.has-element [element]
      (not= nil (. self.__refmap element)))

    ; inserts a new element e with value v immediately after mark and returns
    ; e. If mark is not an element of self, the list is not modified.
    (λ self.insert-after [value mark]
      (when (self.has-element mark)
        (let [element { : value }]
          (if (= mark self.back)
              (do 
                (assert (not mark.__next))
                (set self.back element))
              (do
                (set mark.__next.__prev element)
                (set element.__next mark.__next)))
          (set element.__prev mark)
          (set mark.__next element)
          (tset self.__refmap element true)
          (set self.length (+ self.length 1))
          element)))

    ; inserts a new element e with value v at the front of list l and returns e
    (λ self.push-front [value]
      (let [element { : value }]
        (if self.front
            (do
              (set self.front.__prev element)
              (set element.__next self.front))
            (set self.back element))
        (set self.front element)
        (tset self.__refmap element true)
        (set self.length (+ self.length 1))
        element))

    ; inserts a new element e with value v at the back of list l and returns e
    (λ self.push-back [value]
      (let [element { : value }]
        (if self.back
            (self.insert-after value self.back)
            (self.push-front value))
        element))

    ; inserts a new element e with value v immediately before mark and returns
    ; e. If mark is not an element of self, the list is not modified.
    (λ self.insert-before [value mark]
      (when (self.has-element mark)
        (let [pre mark.__prev] 
          (if pre
              (self.insert-after value pre)
              (self.push-front value)))))

    ; Removes element from self if element is an element of self, returns
    ; element value.
    (λ self.remove [element]
      (when (self.has-element element)
        (let [pre element.__prev
              post element.__next
              value element.value]
          (when (not post) (set self.back pre))
          (when pre (set pre.__next post))
          (when (not pre) (set self.front post))
          (when post (set post.__prev pre))
          (tset self.__refmap element nil)
          (set self.length (- self.length 1))
          value)))

    ; moves element e to its new position after mark. If e or mark is not an
    ; element of self, or e == mark, the list is not modified.
    (λ self.move-after [element mark]
      (when (and (self.has-element element)
                 (self.has-element mark)
                 (not= element mark))
        (self.insert-after (self.remove element) mark)))

    ; moves element e to its new position before mark. If e or mark is not an 
    ; element of self, or e == mark, the list is not modified.
    (λ self.move-before [element mark]
      (when (and (self.has-element element)
                 (self.has-element mark)
                 (not= element mark))
        (self.insert-before (self.remove element) mark)))

    ; moves element e to the back of list l. If e is not an element of self, 
    ; the list is not modified.
    (λ self.move-to-back [element]
      (when (self.has-element element)
        (self.push-back (self.remove element))))

    (λ self.next [_ ?mark]
      (if (and ?mark (self.has-element ?mark))
          ?mark.__next
          self.front))

    (λ self.prev [_ ?mark]
      (if (and ?mark (self.has-element ?mark))
          ?mark.__prev
          self.back))

    ; iterator
    (λ self.elements []
      (values self.next self))

    ; reverse iterator
    (λ self.elements-reverse []
      (values self.prev self))

    ; returns a array-type table of values
    (λ self.as-table []
      (icollect [e (self.elements)] e.value))

    ; inserts a copy of another list at the back of list
    (λ self.push-back-list [alist]
      (each [e (alist.elements)]
        (self.push-back e.value)))

    ; inserts a copy of another list at the front of list
    (λ self.push-front-list [alist]
      (each [e (alist.elements-reverse)]
        (self.push-front e.value)))

    (λ self.has-value [value]
      (var found false)
      (each [e (self.elements) :until found]
        (when (= e.value value)
          (set found true)))
      found)

    (when ?src-table
      (each [_ v (ipairs ?src-table)]
        (self.push-back v)))
    
    ; direct indexing for convenience, should not be used!
    (setmetatable 
      self 
      { :__index 
        (λ [_ n]
          (when (= (type n) :number)
            (if (= n 1)
                self.front
                (= n self.length)
                self.back
                (or (< n 1) (> n self.length))
                nil
                (do
                  (local from-back? (> n (/ self.length 2)))
                  (local iter-fn (if from-back? 
                                     self.elements-reverse
                                     self.elements))
                  (var cnt (if from-back?
                               self.length
                               1))
                  (var last-ref nil)
                  (each [e (self.elements) 
                         :until 
                         (if from-back?
                             (> (- self.length cnt -1) n)
                             (> cnt n))]
                    (set last-ref e)
                    (set cnt (+ cnt (if from-back? -1 1))))
                  last-ref))))})))


