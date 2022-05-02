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
fun Rodear prop padre=
let 
	fun detRodear prop padre=
		case prop of
			constante _
				=> false
			| variable var
				=> false
			| negacion prop
				=> false
			| conjuncion (p1, p2)
				=> (
				case padre of
                    conjuncion (p1, p2) => false
                    | _ => true
				)
			| disyuncion (p1, p2)
				=> (
				case padre of
                    disyuncion (p1, p2) => false
                    | _ => true
				)
			| implicacion (p1, p2)
				=> (
				case padre of
                    implicacion (p1, p2) => false
                    | _ => true
				)
			| equivalencia (p1, p2)
				=> (
				case padre of
                    equivalencia (p1, p2) => false
                    | _ => true
				)
in
	detRodear prop padre
end;

(*
Funcion que devuelve un string la proposicion escrita con variables y simbolos bonitos
Resultado:
    -> String ordenado con las variables y simbolos correspondientes
*)
fun bonita prop=
let
	val par1 = "("
	val par2 = ")"
	fun bonitaR prop=
	  case prop of
	    constante valor
	       => if valor = true then "true"
		      else "false"
	  | variable var
	       => var
	  | negacion prop1
           => let val p1 = bonitaR prop1
                  and simbolo = "~ "
	          in  simbolo ^ p1 
	          end
	  | conjuncion (prop1, prop2)
	       => let val r1 = Rodear prop1 prop
				  val r2 = Rodear prop2 prop
				  val p1 = bonitaR prop1
	              val p2 = bonitaR prop2 
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
	       => let val r1 = Rodear prop1 prop
				  val r2 = Rodear prop2 prop
				  val p1 = bonitaR prop1
	              val p2 = bonitaR prop2 
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
	       => let val r1 = Rodear prop1 prop
				  val r2 = Rodear prop2 prop
				  val p1 = bonitaR prop1
	              val p2 = bonitaR prop2 
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
	       => let val r1 = Rodear prop1 prop
				  val r2 = Rodear prop2 prop
				  val p1 = bonitaR prop1
	              val p2 = bonitaR prop2 
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
    print("Proposicion: " ^ imprimir prop ^ "\nBonita: " ^ bonitaR prop ^ "\n")
end
;