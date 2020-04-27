(define (make-node value action name) (list value action name))
(define (node? n) (list? n))
(define (node-value n) (nth 0 n))
(define (node-action n) (nth 1 n))
(define (node-name n) (nth 2 n))
(define (node-eval n) (if (nil? (node-action n)) (node-value n) (throw-error "unevaluable node")))
(define (merge nodelist)
  (letn
    (priority '(+ - * / ^)
     lowerpri? (lambda (n1 n2) (< (find (node-name n1) priority) (find (node-name n2) priority)))    
     _R (lambda (n1 n2) (make-node ((node-action n1) (node-value n1) (node-value n2)) (node-action n2) (node-name n2)))
     _M (lambda (_l _c)
          (cond
            ((and (empty? _c) (empty? (rest _l))) (first _l))  ; length 1
            ((and (empty? _c) (= 2 (length _l))) (_R (nth 0 _l) (nth 1 _l)))  ; length 2
            ((lowerpri? (nth 0 _l) (nth 1 _l)) (_M (cons (nth 1 _l) (rest (rest _l))) (append _c (list (nth 0 _l)))))
            (true (_M (append _c (cons (_R (nth 0 _l) (nth 1 _l)) (rest (rest _l)))) '())) )))
    (_M nodelist '())))
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
(test "4" (= 4 (evaluate (list (N 4)))))
(test "3+8" (= 11 (evaluate l1)))
(test "3+8*4" (= 35 (evaluate l2)))
(test "2+3*4^2+1" (= 51 (evaluate l3)))
(test "4+2x6-4^3+5" (= -43 (evaluate l4)))
(test "not evaluating 4+" (or (catch (evaluate (list (N 4 +))) 'res) 
                              (= "unevaluable node" ((parse ((parse res "\r") 0) ": ") -1))))
