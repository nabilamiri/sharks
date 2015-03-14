#lang racket
;L ← Empty list that will contain the sorted elements
;S ← Set of all nodes with no incoming edges
;while S is non-empty do
;    remove a node n from S
;    add n to tail of L
;    for each node m with an edge e from n to m do
;        remove edge e from the graph
;        if m has no other incoming edges then
;            insert m into S
;if graph has edges then
;    return error (graph has at least one cycle)
;else 
;    return L (a topologically sorted order)

  
;(define (topoSort a)
;    (if (empty? a #f) ;base case
;    (cons (caar a)) ((topoSort (cdar a) )      ;recursive case which is not done
  
;)))

;check if element a list L contains an element
(define (contains a L)
  (if (eqv? (car L) a) #t
     (if(null? (cdr L)) #f (contains (cdr L) a))
   )
)

;check if vertex a in graph G has indegree greater than 0
(define (indegree a G)
  (if(contains  a (car G)) #t
     (if(null? (cdr G)) #f (contains a (cdr G)))
    )
)
;remove an element from G
(define (remove a G)
  (if (null? G)
      '()
      (if (eqv? a (car G))
          (remove a (cdr G))
          (cons (car G)
                (remove a (cdr G))))))

;get next vertex to remove
(define (getnext G)
  (if(null? G) null ;reached end -> cycle
     (if (indegree car(car G)) ;if head of list has no indegree, return it
         (getnext(cdr G))
         (car(car G)))
))

;topoSort function
(define (topoSort G)
  (if(empty? G) null ;base case
      (if (eqv? (getnext G) null) "Has at least one cycle." ;cyclic
               (cons (getnext G) topoSort(remove (getnext G)))
  ))
)
