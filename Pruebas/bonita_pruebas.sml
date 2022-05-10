(* ---------------- Constantes ----------------*)
val f = constante false;
val t = constante true;

(* ---------------- Variables ----------------*)
val vp = variable "p" ;
val vq = variable "q" ;
val vr = variable "r" ;

(* ---------------- Proposiciones ---------------- *)

(*Con variables*)
val pru1 = vp :||: ~: vp  :&&: vq :||: ~: vq;
val pru2 = vp :&&: (vp :||: vq);
val pru3 = (vp :||: ~: vp)  :&&: (vq :<=>: ~: vq);
val pru4 = vp :&&: vq :=>: vq :||: vp ;
val pru5 = (vp :=>: vq) :&&: (~: vp :=>: vr) :<=>: (vq :||: vr);
(*Ejemplo 1.2 Pagina 63 Libro Matematica Discreta Murillo*)
val pru6 = (vp :=>: (vq :&&: vr)) :&&: (vq :||: vr) :=>: vp;

(*Con constantes*)
val pru7 = f :&&: (t :||: f);
val pru8 = f :&&: t;
val pru9 = t :=>: f :<=>: ~: t :||: f;

(* ---------------- Pruebas ---------------- *)

(*Pruebas con variables*)
bonita pru1;
bonita pru2;
bonita pru3;
bonita pru4;
bonita pru5;
bonita pru6;

(*Pruebas con constantes*)
bonita pru7;
bonita pru8;
bonita pru9;