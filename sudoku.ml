
(* Question 1 *)
(* Type Choisi : une grille est représentée par une matrice 9x9 d'entiers où
   l'index i parcourt les lignes et l'index j parcourt les colonnes.
   Nous avons choisi la valeur 0 pour représenter une case de sudoku vide au moment de l'initialisation.
 *)
type grille = int array array;;
let g : grille =Array.make_matrix 9 9 0 ;;

let case g i j = g.(i).(j);;

(* Fonction d'affichage d'une grille.
  Cette fonction est utilisée dans la fonction test pour assurer
  la bonne correspondance des types *)

let afficher_grille a =
for i = 0 to Array.length a -1 do
        for j = 0 to Array.length a.(0) - 1 do
            print_int a.(i).(j);
            let espace = if ((j+1) mod (Array.length a.(0))) = 0 then "\n" else "\t" in
            print_string espace;
            done;
        done;
    print_string "\n \n";;


let test g =
  afficher_grille g;
  try
	for i=0 to 8 do
		for j=0 to 8 do
      if case g i j < 1 || case g i j > 9 then raise Exit;
      for k=0 to 8 do
        if (k<>j && ((case g i j)=(case g i k))) then raise Exit;
        if (k<>j && ((case g j i)=(case g k i))) then raise Exit;
      done;
    done;
  done;

  for i=0 to 2 do
    for j=0 to 2 do
      for k1=0 to 2 do
        for l1=0 to 2 do
          for k2=0 to 2 do
            for l2=0 to 2 do
              if ((k1<>k2 || l1 <> l2) && ((case g (i*3+k1) (j*3+l1))=(case g (i*3+k2) (j*3+l2)))) then raise Exit;
            done;
          done;
        done;
      done;
    done;
  done;

  true;
  with Exit->false;
;;


(*question 2*)
(* Type Choisi : un problème est représenté par une matrice 9x9 d'entiers où
   l'index i parcourt les lignes et l'index j parcourt les colonnes.
   Nous avons choisi la valeur 0 pour représenter une case de sudoku vide.
*)

type probleme = int array array;;

let p : probleme =Array.make_matrix 9 9 0 ;;

let solution_de p g =
  try
  for i=0 to 8 do
		for j=0 to 8 do
        if ((p.(i).(j)!=0)&&(p.(i).(j)!=case g i j)) then raise Exit;
      done;
    done;

  true;
  with Exit->false;
;;


(* Question 3 *)
(* Type Choisi : une hypothèse est représentée par une matrice 9x9 de listes d'entiers où
   l'index i parcourt les lignes et l'index j parcourt les colonnes.
   Chaque élément est une liste d'entiers correspondant aux hypothèses.
   Une liste avec un seul élément représente une case définitive.
   Une liste vide correspond à une case de sudoku vide.
 *)
 type hypothese = int list array array;;
 let h : hypothese = Array.make_matrix 9 9 [] ;;

 let compatible h g =
   try
   for i=0 to 8 do
 		for j=0 to 8 do
      if (List.length h.(i).(j) > 0) && (List.mem g.(i).(j) h.(i).(j) == false) then raise Exit;
    done;
   done;

   true;
   with Exit->false;
 ;;
