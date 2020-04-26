(define (make-node value action name) (list value action name))
(define (node? n) (list? n))
(define (node-value n) (nth 0 n))
(define (node-action n) (nth 1 n))
(define (node-name n) (nth 2 n))
(define (node-eval n) (if (nil? (node-action n)) (node-value n) (throw-error "unevaluable node")))

; posible mejora: si incluimos la condiicion de reducir cuando nodelist tenga 2 elemntos, no es necesario ç (puede tener cualquier valor,  ej nil) ni incluirlo en la lista de prioridades    
(define (merge nodelist)
  (letn
    (priority '(+ - * / ^)
     lowerpri? (lambda (n1 n2) (< (find (node-name n1) priority) (find (node-name n2) priority)))    
     _R (lambda (n1 n2) (make-node ((node-action n1) (node-value n1) (node-value n2)) (node-action n2) (node-name n2)))
     _M (lambda (_l _c)
         (let (_n (+ _c 1))   
          (cond
            ((empty? (rest _l)) (first _l))  ; length 1
            ((= 2 (length _l)) (_R (nth 0 _l) (nth 1 _l)))  ; length 2
            ((lowerpri? (nth _c _l) (nth _n _l)) (_M _l _n))
            (true (_M (append (slice _l 0 _c) (cons (_R (nth _c _l) (nth _n _l)) (slice _l (+ _n 1))) ) 0))))))
    (_M nodelist 0)))
(define (evaluate nodelist) (let (n (merge nodelist)) (node-eval n)))

;---- testing macro
(define (test test-name test-cond) (if test-cond (println test-name " OK") (println test-name " Fail!!!!")))

;---- macros for easy typing
(define-macro (N v a t) (make-node (eval v) (or (eval a) nil) (or t a nil)))

;---- testing examples
(define l1 (list (N 3 +) (N 8)))
(define l2 (list (N 3 +) (N 8 *) (N 4) ))
(define l3 (list (N 2 +) (N 3 *) (N 4 pow ^) (N 2 +) (N 1) ))
(define l4 (list (N 4 +) (N 2 *) (N 6 -) (N 4 pow ^) (N 3 +) (N 5) ))

;---- tests
(test "3+8" (= 11 (evaluate l1)))
(test "3+8*4" (= 35 (evaluate l2)))
(test "2+3*4^2+1" (= 51 (evaluate l3)))
(test "4+2x6-4^3+5" (= -43 (evaluate l4)))
