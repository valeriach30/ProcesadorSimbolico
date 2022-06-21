exception Error_concatenar of string;

fun concatenar_vals_FND [] n             = raise Error_concatenar "Error lista vacia"
|   concatenar_vals_FND (vb :: vbs) n = 
	let
		val (letra, booleano) = vb
		val var = variable(letra) 
		val varSigno = if booleano then var else (~: var)
	in
		if n > 1 then varSigno :&&: concatenar_vals_FND vbs (n-1) else varSigno
	end
;

exception Error_fnd of string;
fun FND prop =
    let
    	(* variables de trabajo *)
    	val variables = vars prop
    	val n = length variables
    	val lista_combinaciones_booleanas = gen_bools n
    	fun recorrer [] = raise Error_fnd "Error lista vacia"
		|  	recorrer (fila :: mas_filas)  = 
				let
					val asociacion = as_vals variables fila
					val resultado_fila = evalProp asociacion prop
				in
					if resultado_fila then if length(mas_filas) > 0 then (concatenar_vals_FND asociacion n) :||: recorrer mas_filas else (concatenar_vals_FND asociacion n)
					else recorrer mas_filas
				end
    in
        recorrer lista_combinaciones_booleanas
    end
;