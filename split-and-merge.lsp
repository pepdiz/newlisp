; ç signals last node, it's a no-action (it should be last action in node list, indicating last element in parsed string)
; ç should always have the lowest priority
(define (second l) (nth 1 l))
(define (make-action f name) (list f name))
(define (action-f a) (first a))
(define (action-name a) (second a))
(define (action? a) (list? a))
(define (make-node value action) (list value action))
(define (node? n) (list? n))
(define (node-value n) (first n))
(define (node-action n) (second n))
(define (node-eval n) (if (and (nil? (action-f (node-action n))) (= (action-name (node-action n)) 'ç)) (node-value n) (throw-error "unevaluable node")))    
(define (merge nodelist)
  (letn
    (priority '(ç + - * / ^)
     lowerpri? (lambda (n1 n2) (< (find (action-name (node-action n1)) priority) 
                                  (find (action-name (node-action n2)) priority)))    
     reduce (lambda (n1 n2) 
              (make-node ((action-f (node-action n1)) (node-value n1) (node-value n2)) (node-action n2)))
     _M (lambda (_l _c _n)   
          (cond
            ((empty? (rest _l)) (first _l))  ; length 1
            ((lowerpri? (nth _c _l) (nth _n _l)) (_M _l (+ _c 1) (+ _n 1)))
            (true (_M (append (slice _l 0 _c) (cons (reduce (nth _c _l) (nth _n _l)) (slice _l (+ _n 1))) ) 0 1)))))
    (_M nodelist 0 1)))
(define (evaluate nodelist) (let (n (merge nodelist)) (node-eval n)))

;---- testing macro
(define (test test-name test-cond) (if test-cond (println test-name " OK") (println test-name " Fail!!!!")))

;---- actions for easy typing
(define _end_ (make-action nil 'ç))
(define _+_ (make-action + '+))
(define _-_ (make-action - '-))
(define _*_ (make-action * '*))
(define _/_ (make-action / '/))
(define _^_ (make-action pow '^))

;---- testing examples
(define l1 (list (make-node 3 _+_) (make-node 8 _end_)))
(define l2 (list (make-node 3 _+_) (make-node 8 _*_) (make-node 4 _end_) ))
(define l3 (list (make-node 2 _+_) (make-node 3 _*_) (make-node 4 _^_) (make-node 2 _+_) (make-node 1 _end_) ))
(define l4 (list (make-node 4 _+_) (make-node 2 _*_) (make-node 6 _-_) (make-node 4 _^_) (make-node 3 _+_) (make-node 5 _end_) ))

;---- tests
(test "3+8" (= 11 (evaluate l1)))
(test "3+8*4" (= 35 (evaluate l2)))
(test "2+3*4^2+1" (= 51 (evaluate l3)))
(test "4+2x6-4^3+5" (= -43 (evaluate l4)))
