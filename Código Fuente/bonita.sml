(*
Funcion auxiliar para Rodear. Determina el tipo de dato de una proposiciones.
Resultados:
    -> False: Es una constante o una variable. En estos casos se devuelve
    false porque no contienen descendentes y por ende no deben ser encerradas
    por parentesis
    -> True: Es una proposicion, ya sea conjuncion, disyuncion, etc. Se devuelve 
    verdadero porque consiste en dos variables unidas y por ende deben ser 
    encerradas por parentesis
*)
fun Rodear prop=
let 
	fun detRodear prop=
		case prop of
			constante _
				=> false
			| variable var
				=> false
			| negacion prop1
				=> false
			| _ 
				=> true
in
	detRodear prop
end;

(*
Funcion que devuelve una lista con el orden de la proposicion escrita con variables y simbolos
Resultado:
    -> Lista ordenada con las variables y simbolos correspondientes
*)
fun bonita prop=
let
	val par1 = "("
	val par2 = ")"
	fun bonitaR prop =
	  case prop of
	    constante _
	       => " "
	  | variable var
	       => var
	  | negacion prop1
           => let val p1 = bonitaR prop1
                  and simbolo = "~ "
	          in  simbolo ^ p1 
	          end
	  | conjuncion (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
				  val r1 = Rodear prop1
				  val r2 = Rodear prop2
                  and simbolo = " && "
	          in
			  	if r1 = true then
				    (*-------Ambos tienen descendentes-------*)
				    if r2 = true then par1 ^ p1 ^ par2 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Solo la 1ra proposicion tienen descendentes-------*)
					else par1 ^ p1 ^ par2 ^ simbolo ^ p2
			    else 
					(*-------Solo la 2da proposicion tienen descendentes-------*)
					if r2 = true then p1 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Ninguna tienen descendentes-------*)
					else p1 ^ simbolo ^ p2
	          end
	  | disyuncion (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
				  val r1 = Rodear prop1
				  val r2 = Rodear prop2
                  and simbolo = " || "
	          in
			  	if r1 = true then
				    (*-------Ambos tienen descendentes-------*)
				    if r2 = true then par1 ^ p1 ^ par2 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Solo la 1ra proposicion tienen descendentes-------*)
					else par1 ^ p1 ^ par2 ^ simbolo ^ p2
			    else 
					(*-------Solo la 2da proposicion tienen descendentes-------*)
					if r2 = true then p1 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Ninguna tienen descendentes-------*)
					else p1 ^ simbolo ^ p2
	          end
	  | implicacion (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
				  val r1 = Rodear prop1
				  val r2 = Rodear prop2
                  and simbolo = " => "
	          in
			  	if r1 = true then
				    (*-------Ambos tienen descendentes-------*)
				    if r2 = true then par1 ^ p1 ^ par2 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Solo la 1ra proposicion tienen descendentes-------*)
					else par1 ^ p1 ^ par2 ^ simbolo ^ p2
			    else 
					(*-------Solo la 2da proposicion tienen descendentes-------*)
					if r2 = true then p1 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Ninguna tienen descendentes-------*)
					else p1 ^ simbolo ^ p2
	          end
	  | equivalencia (prop1, prop2)
	       => let val p1 = bonitaR prop1
	              val p2 = bonitaR prop2
				  val r1 = Rodear prop1
				  val r2 = Rodear prop2
                  and simbolo = " <=> "
	          in
			  	if r1 = true then
				    (*-------Ambos tienen descendentes-------*)
				    if r2 = true then par1 ^ p1 ^ par2 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Solo la 1ra proposicion tienen descendentes-------*)
					else par1 ^ p1 ^ par2 ^ simbolo ^ p2
			    else 
					(*-------Solo la 2da proposicion tienen descendentes-------*)
					if r2 = true then p1 ^ simbolo ^ par1 ^ p2 ^ par2
					(*-------Ninguna tienen descendentes-------*)
					else p1 ^ simbolo ^ p2
	          end
in
    bonitaR prop (* elimina valores repetidos *)
end
;