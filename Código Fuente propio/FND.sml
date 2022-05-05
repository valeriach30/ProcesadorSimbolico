fun imprimir_vals_FND [] n             = ")"
|   imprimir_vals_FND ((v,b) :: vbs) n = 
	if n > 1 then (if b then v ^ " :&&: " ^ imprimir_vals_FND vbs (n-1) else "~:" ^ v  ^ " :&&: " ^ imprimir_vals_FND vbs (n-1))
	else (if b then v ^ imprimir_vals_FND vbs (n-1) else "~:" ^ v  ^ imprimir_vals_FND vbs (n-1))

;

fun FND prop =
    let
    	(* variables de trabajo *)
    	val variables = vars prop
    	val n = length variables
    	val lista_combinaciones_booleanas = gen_bools n
    	(* imprimir una fila de la tabla de verdad *)
    	fun imprimir_fila vars_bools es_verdadero isPrimero =
			case isPrimero of
			   0 => print( if es_verdadero then " :||: (" ^ imprimir_vals_FND vars_bools n else "") 
			 | 1 => print( if es_verdadero then "(" ^ imprimir_vals_FND vars_bools n else "") 
        (* generar evaluaciones de la proposición*)
    	fun recorrer [] isPrimero        = print "\n"  (* toque final a la impresión; previamente mostramos hileras con el resultado *)
		|   recorrer (fila :: mas_filas) isPrimero = 
    		    let
		        	(* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
 	    		    val resultado_fila = evalProp asociacion prop
                in
            	    imprimir_fila  asociacion  resultado_fila isPrimero(* efecto: imprimir fila y su evaluación *)
            	    ;
            	    recorrer mas_filas 0(* continuar el trabajo *)
            	end
		
    in
        recorrer lista_combinaciones_booleanas 1
    end
;


val pru1 = ~:(variable "p") :&&: (variable "q") ;
val pru2 = (variable "q") :=>: (variable "p") ;
val pru3 = pru1 :=>: pru2 ;
FND pru3;