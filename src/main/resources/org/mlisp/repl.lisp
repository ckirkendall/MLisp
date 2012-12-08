(println "Welcome to the CKLisp REPL!")

(defn seval (exp) 
  (let (env *coreenv*
        reader *reader*
        pexp (. reader parseAll (. reader exp) exp))
    (. org.mlisp.Eval eval (. pexp get) env)))

(defn readloop ()
  (print "mlisp>")
  (let (exp (readln))
    (try
    	(println (seval exp))
    	(catch (error java.lang.Throwable) 
    	  (. *coreenv* assign '*exception* error) 
    	  (println "error found"))))
  (readloop))

(readloop)