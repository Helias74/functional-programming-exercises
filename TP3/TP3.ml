(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

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
    |ABNoeud (v, fg, fd) -> if x<v then insere_arbre_bin_recherche (x)(fg)
        else if x>v then insere_arbre_bin_recherche (x)(fd)
        else a;; (*On ne fait rien si l'élément est dans l'arbre, on renvoie le même arbre *)
  
                 
                 
  let rec list_of_arbre_bin (a : arbre_bin): int list =
    match a with 
    |ABVide -> []
    |ABNoeud (v, fg, fd) -> concatene (concatene (list_of_arbre_bin fg) [v]) (list_of_arbre_bin fd);;