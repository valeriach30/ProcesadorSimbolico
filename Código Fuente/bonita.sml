(*conjuncion(variable "a", variable "b")*)
fun bonita prop=
let
	fun bonitaR prop =
	  case prop of
	    constante _
	       => []
	  | variable var
	       => [var]
	  | negacion prop1
           => let val p1 = bonitaR prop1
                  and simbolo = ["~"]
	          in  simbolo @ p1 
	          end
	  | conjuncion (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
                  and simbolo = ["&&"]
	          in  p1 @ simbolo @ p2
	          end
	  | disyuncion (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
                  and simbolo = ["||"]
	          in  p1 @ simbolo @ p2
	          end
	  | implicacion (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
                  and simbolo = ["=>"]
	          in  p1 @ simbolo @ p2
	          end
	  | equivalencia (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
                  and simbolo = ["<=>"]
	          in  p1 @ simbolo @ p2
	          end
in
    bonitaR prop (* elimina valores repetidos *)
end
;