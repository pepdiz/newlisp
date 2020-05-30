
;building split function for split&merge algotithm using TDD

;test function 
(define (test n e) (if e (println (append n ": OK")) (println (append n ": Fail!!"))))

;token datatype and accesors
(define (make-token val acc)  (list val acc))
(define (token-val t) (t 0))
(define (token-acc t) (t 1))
(define (token? t) (and (list? t) (= 2 (length t))))

;auxiliary functions
(define (numero? snum) (regex {^[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$} snum))
(define (numero snum) (if (regex {^[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$} snum) (float $0) nil))
(define (opind cad) (let (OPER {[\+\-\*\/]}) (find OPER cad 0)))
(define (eliminar-espacios c) (replace {[ \t]+} c "" 0))
 
;split function generate tokens built as a value and an operator
;original split&merge algorithm is intented to deal with mathematical expressions (numbers and operators) only
;further modifications allow using more complex expressions like functions and other datatypes
;I'll try to extend it to allow variables, user defined functions and so on
(define (split cad) 
 (letn (str (eliminar-espacios cad) OP (opind str) LNUM (and OP (numero (slice str 0 OP))))
   (cond 
    ((empty? str) '())
    ((numero? str) (list (make-token (float str) nil)))  
    ((<= OP 0) (throw-error (format "%s is not valid" str))) ; creo que puede eliminase y asumirse en la true 
    (LNUM (cons (make-token LNUM (sym (str OP))) (split (slice str (+ OP 1)))))
    (true     (throw-error (format "%s is not valid" str))) )))

(test "cadena vacia no tiene tokens" (= (split "") '()))
(test "1 es token valido" (= (split "1") (list (make-token 1 nil))))
(test "-1 es token valido" (= (split "-1") (list (make-token -1))))
(test "numero es token valido" (= (split "10") (list (make-token 10))))
(test "real es token valido" (= (split "4.5") (list (make-token 4.5))))
(test "notacion cientifica valida" (= (split "-45.78e2") (list (make-token -4578))))
(test "suma no es token valido" (nil? (catch (split "+") 'err)))
(test "1+2 es valido" (= (split "1+2") (list (make-token 1 '+) (make-token 2)))) 
(test "multiplicacion no es token valido" (nil? (catch (split "*") 'err)))
(test "1*2 es valido" (= (split "1*2") (list (make-token 1 '*) (make-token 2)))) 
(test "aplicacion de operador es valido" (= (split "1.45/-2e3") (list (make-token 1.45 '/) (make-token -2000)))) 
(test "espacios suprimidos" (= (split "    1  ") (make-token 1)))
(test "espacios suprimidos entre numeros" (= (split "  2  35  .6e  + 2") (list (make-token 23560))))
(test "encadenar operaciones es valido" (= (split "1+2*6") (list (make-token 1 '+) (make-token 2 '*) (make-token 6))))
