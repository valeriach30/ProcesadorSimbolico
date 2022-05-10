(*Se usan dos variables para ver la reaccion 
de la funcion al lidiar con dos variables iguales y
con dos diferentes..*)
val pru1 = (variable "a") :&&: (variable "b") ;
vars (pru1);
val pru2 = (variable "x") :&&: (variable "x") ;
vars pru2;