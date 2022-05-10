(*En esta prueba se utilizan dos varianles para
poder llamar a la funcion y ver su reaccion*)

val pru1 = (variable "a") :&&: (variable "b") ;
vars pru1;
val booleanos = gen_bools 2;
exception Head;
fun head[] = raise Head | head (x :: xs) = x;
as_vals (vars pru1) (head booleanos);