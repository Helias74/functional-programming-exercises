
let rec concatene (l1 : int list) (l2 : int list) : int list = 
  match l1 with 
  | [] -> l2 
  | hd :: tl -> hd :: (concatene tl l2);;(*hd 1ere élément de la liste et tl donne le reste de la liste*)
  
  
  
  
  
type arbre_bin =
  | ABVide
  | ABNoeud of int * arbre_bin * arbre_bin ;;
  
(* Arbres pour tests *)
  
  
let ABVide = ABVide;;
  
(* Arbre avec un seul nœud (racine = 5) *)
let ab1 = ABNoeud (5, ABVide, ABVide);;
  
      
(* générer chat gpt *)
let ab2 = ABNoeud (20, ABNoeud (10, ABNoeud (5, ABVide, ABVide), ABVide), ABNoeud (30, ABVide, ABNoeud (35, ABVide, ABVide)));;
  
(*Début exo*)
  
let rec taille_ab (a : arbre_bin): int =
  match a with 
  |ABVide -> 0 
  |ABNoeud (v, fg, fd) -> 1 + taille_ab (fg) +  taille_ab (fd);;
  
  
assert (taille_ab ABVide = 0);;
assert (taille_ab ab1 = 1);;
taille_ab ab2 ;;
  
let rec produit_ab (a : arbre_bin): int =
  match a with 
  |ABVide -> 1 
  |ABNoeud (v, fg, fd) -> v * produit_ab (fg) *  produit_ab (fd);;
  
assert (produit_ab ABVide = 1);;
produit_ab ab1;;
produit_ab ab2 ;;
  
  
  
let rec insere_arbre_bin_recherche (x : int )(a : arbre_bin): arbre_bin =
  match a with 
  |ABVide ->  ABNoeud (x, ABVide, ABVide)
  |ABNoeud (v, fg, fd) -> if x<v then ABNoeud (v, (insere_arbre_bin_recherche (x)(fg)), fd)
    else if x>v then ABNoeud (v, fg, (insere_arbre_bin_recherche (x)(fd)))
    else a;; (*On ne fait rien si l'élément est dans l'arbre, on renvoie le même arbre *)
  

(*Deux arbre pour faire un test avec assert *)

(*Première arbre*)
let arbre_initial = 
  ABNoeud (10, 
  ABNoeud (5, ABVide, ABVide), 
  ABNoeud (15, ABVide, ABVide)
);;                
        
(*arbre identique mais avec 7 rajouté*)

let arbre_attendu = 
  ABNoeud (10, 
  ABNoeud (5, ABVide, ABNoeud (7, ABVide, ABVide)), 
  ABNoeud (15, ABVide, ABVide)
);;


assert(insere_arbre_bin_recherche (7)(arbre_initial) = arbre_attendu);;
insere_arbre_bin_recherche (7)(arbre_initial) = arbre_attendu;;(*Reviens au même mais renvoie true*)



let rec list_of_arbre_bin (a : arbre_bin): int list =
  match a with 
  |ABVide -> []
  |ABNoeud (v, fg, fd) -> concatene (concatene (list_of_arbre_bin fg) [v]) (list_of_arbre_bin fd);;


list_of_arbre_bin (arbre_attendu);;



let rec arbre_bin_rech_of_int_list (l :int list) : arbre_bin = 
  match l with
  |[] -> ABVide
  |tl :: sl -> insere_arbre_bin_recherche tl ( arbre_bin_rech_of_int_list sl );;


(*Ici l'assert retourne une erreur -> l'arbre binaire n'est formé n'est pas équlibré *)

arbre_bin_rech_of_int_list([20; 10; 11;10;48; 15]);; (*Pour vérifier si donne bien un ABR*)
assert( arbre_attendu = arbre_bin_rech_of_int_list([5; 7; 10; 15]));;


(*PARTIE DEUX *)


type binop = Plus | Moins | Mult;;

type expr =
  | Cst of int
  | Binop of binop * expr * expr;;


let expr1 = Binop (Plus, Cst 3, Cst 4);;
let expr2 = Binop (Plus, Binop(Moins,Cst 2,Cst 3), Cst 4);;


let rec string_of_expr (e : expr) : string =
  match e with
  |Cst n -> string_of_int n
  |Binop (op, e1, e2)-> let op_str (op : binop) : string = match op with
                      |Plus -> "+"
                      |Moins -> "-"
                      |Mult -> "*" in 
                      "(" ^ (string_of_expr e1) ^ " " ^ op_str (op) ^ " " ^ (string_of_expr e2) ^ ")" ;;(* ^ pour concaténer les chaines de char *)


string_of_expr (expr1);;
string_of_expr (expr2);;