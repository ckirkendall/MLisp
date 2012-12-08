(def true (. java.lang.Boolean TRUE))
(def false (. java.lang.Boolean FALSE))
(def cons (fn (item lst) (. lst $colon$colon item)))
(def first (fn (lst) (. lst head)))
(def rest (fn (lst) (. lst tail)))
(def empty? (fn (x) (. x isEmpty)))

(def let* 
  (macro (plst & body)
    (cons (cons 'fn 
		(cons (cons (first plst) ()) 
		      body)) 
	  (rest plst))))
	  
(def let 
  (macro (plst & body)
    (if (empty? plst)
        (cons (cons 'fn (cons () body)) ())
        (cons 'let* 
               (cons (cons (first plst) 
			   (cons (first (rest plst)) ()))
		     (cons (cons 'let 
				 (cons (rest (rest plst)) 
				       body)) 
			   ()))))))

(def defmacro 
  (macro (sym & body)
    (let (m (cons 'macro body))
      '(def ~sym ~m))))

(defmacro defn (sym & body) 
  (let (f (cons 'fn body)) 
  	'(def ~sym ~f)))

(def sysin (new java.io.BufferedReader (new java.io.InputStreamReader (. java.lang.System in))))
(defn println (x) (. (. java.lang.System out) println x))
(defn print (x) (. (. java.lang.System out) print x))
(defn read () (. sysin read))
(defn readln () (. sysin readLine))

(defn + (x y) (. org.mlisp.utils.Math add x y))
(defn - (x y) (. org.mlisp.utils.Math sub x y))