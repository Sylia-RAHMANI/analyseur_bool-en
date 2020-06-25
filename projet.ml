



			(****				SOLVEUR BOOLEEN					*****)

(*ce programme est une implémentation d'un algorithme qui permet de calculer l'ensemble de solutions d'un système d'équations booléen*)


(*définition du tuype eb*)
type eb = V of int |TRUE|FALSE|AND of eb* eb |OR of eb * eb|XOR of eb * eb|NOT of eb;; 


(*fonction qui renvoie true si une variable (V(n)) appartient à une liste donnée et renvoie false sinon*)
(*paramettres :
 l : une liste de variables.  
 x :l'indice de la variable V(x)*)

let rec appartient x l= match l with
		|[]->false
		|V(m)::b -> if m=x then true else appartient x b
		|_->false;;
(*	val appartient : int -> eb list -> bool = <fun>		*)

let e =appartient 30 [V(1);V(3);V(8)];;

(* val e : bool = false  *)


(*fonction qui détermine l'ensemble des variables d'une équation donnée en paramettre et les ajoute à une liste qui serai renvoyée*)

let rec determine q l = match q with
	|V(a)-> if (appartient a l) then l else V(a)::l 
	|AND(c,b)-> determine c (determine b l)
	|OR (c,b) -> determine c (determine b l)
	|XOR (c,b) -> determine c (determine b l)
	|NOT(c) -> (determine c l)
	|_ -> l;;

let ll= determine ( OR( V(1),NOT(  AND(V(3), XOR(V(1),V(3)) ) )  )  ) [];;


(* val determine : eb -> eb list -> eb list = <fun> *)
(* val ll : eb list = [V 1; V 3] *)





(*fonction qui détermine l'ensemble des variables d'un système d'équations représenté comme une liste de couples (équation1,équation2) qui veut dire équation1 = équation2 . EXEMPLE: 
le système {V1 OR V2 = TRUE; NOT(V3 XOR V1)=FALSE} est représenté ici comme une liste ([(OR(V(1),V(2)),TRUE) ; (NOT(XOR(V(3),V(1))),FALSE)] **)

let rec ensble_var l = match l with
	|[] ->[]
	|(a,b)::p -> determine a ( (determine b (ensble_var p)));;

let r= ensble_var [(OR(V(1),V(2)),TRUE);(XOR(V(0),V(3)),V(1)) ; (NOT (AND(V(5),(AND(V(6),V(1))))),TRUE)];;

(*   val ensble_var : (eb * eb) list -> eb list = <fun>
     val r : eb list = [V 2; V 0; V 3; V 5; V 6; V 1] *)




(*Importation du type arbre pour représenter un arbre de tout les cas possibles de valeurs de vérité d'une listes de variables*)
type arbre= Vide | Noeud of (eb*eb) * arbre * arbre ;;




(*fonction qui insere un Noeud donné en paramettre dans un arbre donné et met une racine qui a une valeur de véritée TRUE*)
 
let rec inserer_arbre_TRUE n a = match a with
	|Vide-> Noeud((V(n),TRUE),Vide,Vide)
	|Noeud(x,fg,fd) ->if(fg=Vide) then (Noeud(x,Noeud((V(n),TRUE),Vide,Vide),Noeud((V(n),FALSE),Vide,Vide)) )  else 
		         	Noeud(x, (inserer_arbre_TRUE n fg), (inserer_arbre_TRUE n fd));;


(* val inserer_arbre_TRUE : int -> arbre -> arbre = <fun> *)


(*fonction qui insere un Noeud donné en paramettre dans un arbre donné et si c'est une racine il lui insere une valeur de véritée FALSE*)
 
let rec inserer_arbre_FALSE n a = match a with
	|Vide-> Noeud((V(n),FALSE),Vide,Vide)
	|Noeud(x,fg,fd) ->if(fg=Vide) then (Noeud(x,Noeud((V(n),TRUE),Vide,Vide),Noeud((V(n),FALSE),Vide,Vide)) )  else 
		         	Noeud(x, (inserer_arbre_FALSE n fg), (inserer_arbre_FALSE n fd));;

(*      val inserer_arbre_FALSE : int -> arbre -> arbre = <fun>    *)



(* fonction qui génère un arbre de racine TRUE à partir d'une liste de valeurs V(n) et met touts les cas possibles pour ce cas*)

let rec generateur_racine_TRUE l a = match l with
		|[] -> a
		|V(n)::q-> generateur_racine_TRUE q (inserer_arbre_TRUE n a)
		|_->a;;

(*val generateur_racine_TRUE : eb list -> arbre -> arbre = <fun>*)
let g= generateur_racine_TRUE ([V(1);V(2);V(3)]) Vide;;

(*val g : arbre =
  Noeud ((V 1, TRUE),
    Noeud ((V 2, TRUE), Noeud ((V 3, TRUE), Vide, Vide),
   	 Noeud ((V 3, FALSE), Vide, Vide)),
   		Noeud ((V 2, FALSE), Noeud ((V 3, TRUE), Vide, Vide),
    			Noeud ((V 3, FALSE), Vide, Vide)))
*)

(* fonction qui génère un arbre de racine FALSE à partir d'une liste de valeurs V(n) et met touts les cas possibles pour ce cas*)

let rec generateur_racine_FALSE l a = match l with
		|[] -> a
		|V(n)::q-> generateur_racine_FALSE q (inserer_arbre_FALSE n a)
		|_->a;;

(*val generateur_racine_FALSE : eb list -> arbre -> arbre = <fun>*)

let g= generateur_racine_FALSE ([V(1);V(2);V(3)]) Vide;;


(*  val g : arbre =
  Noeud ((V 1, FALSE),
   	Noeud ((V 2, TRUE), Noeud ((V 3, TRUE), Vide, Vide),
    		Noeud ((V 3, FALSE), Vide, Vide)),
  			 Noeud ((V 2, FALSE), Noeud ((V 3, TRUE), Vide, Vide),
   				 Noeud ((V 3, FALSE), Vide, Vide)))
*)

(*fonction qui génere tout les chemins qui existent de la racine jusqu'à la feuille et met chaque chemin dans une liste qui est renvoyée à la fin*)
let rec arbredeliste a l= match a with
	| Vide -> l
	| Noeud(e,Vide,Vide) -> e::[]
	| Noeud (e,fg,fd) -> e::(arbredeliste fg (e::l))@l@e:: (arbredeliste fd (e::l));;


(* val arbredeliste : arbre -> (eb * eb) list -> (eb * eb) list = <fun>*)

let k=arbredeliste (generateur_racine_TRUE ([V(1);V(2);V(3)]) Vide) [];;

(*val k : (eb * eb) list =
	[(V 1, TRUE); (V 2, TRUE); (V 3, TRUE); (V 1, TRUE); (V 2, TRUE);
   	(V 3, FALSE); (V 1, TRUE); (V 2, FALSE); (V 3, TRUE); (V 1, TRUE); (V 2, FALSE); (V 3, FALSE)]
*)





(*fonction de concaténation de deux listes*)

let  rec concat l1 l2 =match l1 with
		|[]->l2
		|x1::ll1 -> x1:: concat ll1 l2 ;;

(*val concat : 'a list -> 'a list -> 'a list = <fun>
*)
let z=concat (arbredeliste (generateur_racine_TRUE ([V(1);V(2);V(3)]) Vide) []) (arbredeliste (generateur_racine_FALSE ([V(1);V(2);V(3)]) Vide) []);;

(*val z : (eb * eb) list =
  [(V 1, TRUE); (V 2, TRUE); (V 3, TRUE); (V 1, TRUE); (V 2, TRUE);
   (V 3, FALSE); (V 1, TRUE); (V 2, FALSE); (V 3, TRUE); (V 1, TRUE);
   (V 2, FALSE); (V 3, FALSE); (V 1, FALSE); (V 2, TRUE); (V 3, TRUE);
   (V 1, FALSE); (V 2, TRUE); (V 3, FALSE); (V 1, FALSE); (V 2, FALSE);
   (V 3, TRUE); (V 1, FALSE); (V 2, FALSE); (V 3, FALSE)]
*)




(*géneration d'une liste qui contient enfin touts les cas possibles de valeurs de vérité de variables en concaténant les deux listes résultats des deux arbres de racines TRUE et FAlSE*)

let rec listeComplete l=concat (arbredeliste (generateur_racine_TRUE l Vide) []) (arbredeliste (generateur_racine_FALSE l Vide) []);;
(*eb list -> (eb * eb) list = <fun>*)


let t=listeComplete [V(1);V(2);V(3)];;
(*val t : (eb * eb) list =
  [(V 1, TRUE); (V 2, TRUE); (V 3, TRUE); (V 1, TRUE); (V 2, TRUE);
   (V 3, FALSE); (V 1, TRUE); (V 2, FALSE); (V 3, TRUE); (V 1, TRUE);
   (V 2, FALSE); (V 3, FALSE); (V 1, FALSE); (V 2, TRUE); (V 3, TRUE);
   (V 1, FALSE); (V 2, TRUE); (V 3, FALSE); (V 1, FALSE); (V 2, FALSE);
   (V 3, TRUE); (V 1, FALSE); (V 2, FALSE); (V 3, FALSE)]
*)


let rec decoupTT l p = match (l,p) with 
		|([],_)->[],[]
		|(V(x)::ll,V(t)) -> let a,b =decoupTT ll p in if x<t then V(x)::a,b else a ,V(x)::b 
		|_->[],[];;
(*val decoupTT : eb list -> eb -> eb list * eb list = <fun>*)

(*fonction qui trie une liste *)
let rec triT l=match l with 
	[]->[]
	|x::ll -> let a,b=decoupTT ll x in (triT a)@(x::(triT b));;
(*val triT : eb list -> eb list = <fun>*)


let r=triT( ensble_var [(OR(V(1),V(2)),TRUE);(XOR(V(0),V(3)),V(1)) ; (NOT (AND(V(5),(AND(V(6),V(1))))),TRUE)]);;
(*val r : eb list = [V 0; V 1; V 2; V 3; V 5; V 6]*)

(*fonction qui enleve autant de nombre de premiers elements q'il y a de variables  et les renvoie dans une liste*)
let rec enleve_premiere_liste l ll= match l with 
		|[] ->[]
		|a::q -> match ll with 
				|o::p ->o::enleve_premiere_liste q p
				|_->[];;
			
(*val enleve_premiere_liste : 'a list -> 'b list -> 'b list = <fun>*)


let f=enleve_premiere_liste [V(1);V(2);V(3);V(8)] (listeComplete [V(1);V(2);V(3)]);;
(*   val f : (eb * eb) list = [(V 1, TRUE); (V 2, TRUE); (V 3, TRUE); (V 1, TRUE)]*)


(*fonction qui supprime autant de nombre de premiers elements q'il y a de variables  et renvoie la nouvelle liste *)
let rec supprime_premiere_liste ll lll= match (ll,lll) with
			|([],p)->p
			|(a::q,b::w) ->supprime_premiere_liste q w
			|_ ->lll;;
			
		
(*val supprime_premiere_liste : 'a list -> 'b list -> 'b list = <fun>*)
	
let rec s= supprime_premiere_liste [V(1);V(2);V(3)] (listeComplete [V(1);V(2);V(3)]);;
(*      
val s : (eb * eb) list =
  [(V 1, TRUE); (V 2, TRUE); (V 3, FALSE); (V 1, TRUE); (V 2, FALSE);
   (V 3, TRUE); (V 1, TRUE); (V 2, FALSE); (V 3, FALSE); (V 1, FALSE);
   (V 2, TRUE); (V 3, TRUE); (V 1, FALSE); (V 2, TRUE); (V 3, FALSE);
   (V 1, FALSE); (V 2, FALSE); (V 3, TRUE); (V 1, FALSE); (V 2, FALSE);
   (V 3, FALSE)]
*)


(*cette fonction divise la liste en sous listes des cas possibles de valeurs de variables et renvoie une liste de sous listes*)
let rec divise_liste ll lll= match lll with 
		|[]->[]
		|_ ->(enleve_premiere_liste ll lll) :: divise_liste  ll (supprime_premiere_liste ll lll);;	

let y= divise_liste [V(1);V(2);V(3)] (listeComplete [V(1);V(2);V(3)]);;
  (*  val divise_liste : 'a list -> 'b list -> 'b list list = <fun>*)
(*  val y : (eb * eb) list list =
  [[(V 1, TRUE); (V 2, TRUE); (V 3, TRUE)];
   [(V 1, TRUE); (V 2, TRUE); (V 3, FALSE)];
   [(V 1, TRUE); (V 2, FALSE); (V 3, TRUE)];
   [(V 1, TRUE); (V 2, FALSE); (V 3, FALSE)];
   [(V 1, FALSE); (V 2, TRUE); (V 3, TRUE)];
   [(V 1, FALSE); (V 2, TRUE); (V 3, FALSE)];
   [(V 1, FALSE); (V 2, FALSE); (V 3, TRUE)];
   [(V 1, FALSE); (V 2, FALSE); (V 3, FALSE)]]*)



(*evaluation d'une expression OR(a,b) *)

let rec evaluer_OR (a,b)= match (a,b) with 
			|(TRUE,_) -> TRUE
			|(_,TRUE) ->TRUE
			|_ -> FALSE;;


(*evaluation d'une expression AND(a,b) *)

let rec evaluer_AND (a,b)= match (a,b) with 
			|(TRUE,TRUE) -> TRUE
			|_-> FALSE
			

(*evaluation d'une expression XOR(a,b)*)

let rec evaluer_XOR (a,b) = if a=b then FALSE else TRUE;; 



(*evaluation d'une expression NOT(a,b)*)

let rec evaluer_NOT p =  match p with 
			|TRUE-> FALSE
			|_->TRUE;;
		

(*evaluation d'une expression complete*)

(*CETTE FONCTION SERT À SAVOIR LA VALEUR DE VIRITÉ D'UNE VARIABLE DONNÉ*)
let rec valeur_de_virite e l =
	 match l with
	 | []-> failwith "erreur"
	 | (m,v)::r -> if (e = m) then v else valeur_de_virite e r ;;

(*'a -> ('a * 'b) list -> 'b = <fun>*)

let l =[(V(1),TRUE);(V(2),FALSE)];;
let b = valeur_de_virite (V(1)) l ;;
b;;

(*val b : eb = TRUE *)


(*CETTE FONCTION SERT A RENVOYER LA VALEUR DE VÉRITER D'UNE EXPRISSION BOOLÉAN*)

let rec valeur_de_verite_exp a i = 
	match a with
	|TRUE->TRUE
	|FALSE->FALSE
	|V(n)-> valeur_de_virite (V(n)) i
	|NOT(x)-> evaluer_NOT(valeur_de_verite_exp x i)
	|AND(x,y)-> evaluer_AND(valeur_de_verite_exp x i,valeur_de_verite_exp y i)
	|OR(x,y)-> evaluer_OR (valeur_de_verite_exp x i ,valeur_de_verite_exp y i)
	|XOR(x,y)-> evaluer_XOR(valeur_de_verite_exp x i ,valeur_de_verite_exp y i);;


(*val valeur_de_verite_exp : eb -> (eb * eb) list -> eb = <fun> *)
let al = NOT(AND(V(1),V(2)));;
let d = valeur_de_verite_exp al [(V(1),TRUE);(V(2),FALSE)];; 
 d;;
(*val al : eb = NOT (AND (V 1, V 2))
val d : eb = TRUE
- : eb = TRUE
*)
(*CETTE FONCTION SERT A SAVOIR SI LES VALEUR DE VERITÉ QUI SE TOUVE DANS LA LISTE i VERIFIE L'EQUATION a=b*)
let rec equation_est_satisfaite (a,b) i = if((valeur_de_verite_exp a i)= (valeur_de_verite_exp b i))
											then TRUE
										    else FALSE;;
(*val equation_est_satisfaite : eb * eb -> (eb * eb) list -> bool = <fun> *)

let e = equation_est_satisfaite (al,TRUE) [(V(1),TRUE);(V(2),FALSE)];;
e;;

(*
val e : bool = true
- : bool = true 
*)
let f =	equation_est_satisfaite (al,FALSE) [(V(1),TRUE);(V(2),FALSE)];;	 
f;;									
(*
	val f : bool = false
- : bool = false
*)

(*CETTE FONCTION SERT À SAVOIR SI LES VALEUR DE VERITÉ QUI SE TROUVE DANS LA LISTE i(ICI I EST UN SEUL MAILLONT) VERIFIE TOUS LE SYSTÉME*)
let rec systeme_est_satisfait l i = 
	match l with 
	|[] -> FALSE
	|(a,b)::[]->if (equation_est_satisfaite (a,b) i)=TRUE then TRUE else FALSE
	|(a,b)::q -> if(equation_est_satisfaite (a,b) i)=TRUE then systeme_est_satisfait q i else FALSE;;
let sys = systeme_est_satisfait [(AND(V(1),V(2)),TRUE);(OR(V(1),V(1)),V(1))] [(V(1),TRUE);(V(2),FALSE)];;
sys;;
(*val sys : eb = FALSE
- : eb = FALSE
# 
*)
let sy = systeme_est_satisfait [(AND(V(1),V(1)),TRUE);(OR(V(1),V(1)),V(1))] [(V(1),TRUE);(V(2),FALSE)];;
sy;;

(*val sy : eb = TRUE
- : eb = TRUE
*)

let ss = systeme_est_satisfait [(AND(V(1),V(1)),TRUE);(OR(V(2),V(2)),V(1))] [(V(1),TRUE);(V(2),FALSE)];;
ss;;
(*val ss : eb = FALSE
- : eb = FALSE
*)

(*cette fonction evalue un systeme et donne une liste de listes  d'ensemble de solutions qui satisfaits le système*)
let rec ensemble_de_solution l i = (*l=liste d'equation i=liste de toute les combinaison possible*)
     match (l,i) with
     |(((a,b)::[]),t::[])->if ((equation_est_satisfaite (a,b) t)= TRUE) then t::[] else []
     | (d,b::t)->if((systeme_est_satisfait d b) == TRUE) then b::(ensemble_de_solution d t)
 		 else ensemble_de_solution d t
     | (_,_)-> [] ;;	


(*EXEMPLE DU DEVOIR *)

let c=ensemble_de_solution [(OR(V(1),V(2)),TRUE);(XOR(V(1),V(3)),V(2));(NOT(AND(V(1),AND(V(2),V(3)))),TRUE)] [[(V 1, TRUE); (V 2, TRUE); (V 3, TRUE)];[(V 1, TRUE); (V 2, TRUE); (V 3, FALSE)];[(V 1, TRUE); (V 2, FALSE); (V 3, TRUE)];[(V 1, TRUE); (V 2, FALSE); (V 3, FALSE)];[(V 1, FALSE); (V 2, TRUE); (V 3, TRUE)];[(V 1, FALSE); (V 2, TRUE); (V 3, FALSE)];[(V 1, FALSE); (V 2, FALSE); (V 3, TRUE)];[(V 1, FALSE); (V 2, FALSE); (V 3, FALSE)]];;


let rec projet l = ensemble_de_solution l ( divise_liste(  triT( 	ensble_var(l)))  (listeComplete (triT( ensble_var l))));;
(*val projet : (eb * eb) list -> (eb * eb) list list = <fun>*)


 									

let v=	projet [(OR(V(1),V(2)),TRUE);(XOR(V(1),V(3)),V(2));(NOT(AND(V(1),AND(V(2),V(3)))),TRUE)];;	

(*val v : (eb * eb) list list =
  [[(V 1, TRUE); (V 2, TRUE); (V 3, FALSE)];
   [(V 1, TRUE); (V 2, FALSE); (V 3, TRUE)];
   [(V 1, FALSE); (V 2, TRUE); (V 3, TRUE)]]*)






							
