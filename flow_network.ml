(*Importation du module graph*)
open Graph

type ford_graph = (int*int) graph

type chemin = id list

type path_list = chemin list

type forbidden = id list (*liste des noeuds deja visités*)

(*on initialise le graphe*)

let init graph = map graph (fun x -> (0,int_of_string x)) (*On met les arcs à 0 de flot*)

(*Verifie s'il reste de la place sur un arc*)
let arc_dispo (id,(flot,capacite)) = 
	if flot < capacite then true else false

(*fonction pour trouver tous les chemins dans le graphe*)
let find_path graph origine arrivee = 
	let rec loop depart accu forbidden=
		if depart = arrivee then
			Some(List.rev accu) (*inverse la liste de retour pour avoir les noeuds dans l'ordre*) 
		else
			let arcs = List.filter arc_dispo (out_arcs graph depart)  (*On filtre les arcs de sortie du noeud depart pour ne garder que ceux dont la capa n'est pas pleine*)
			in
			let rec parcours_arcs liste_arcs = match liste_arcs with
				|[] -> Some []  (*remplacer par None pour retrouver version de jerome*)
				|(id,(flot,capacite))::rest -> 
					if List.mem id forbidden then
						parcours_arcs rest		(*Si l'arc a été visité on ne le parcourt pas*) (*a remplacer par Some [] ou None*) (*remplacer par None pour retrouver version de jerome*)
					else
						let chemin = loop id (id::accu) (id::forbidden)
						in
						begin
							match chemin with 
								|Some [] -> parcours_arcs rest (*Si le chemin ne mène à rien on essaye un autre arc*)
								| _ -> chemin (*Sinon on renvoie le chemin trouvé*)
						end

			in
			parcours_arcs arcs
	in loop origine [origine] [origine]

let transforme truc = match truc with
	|Some x-> x
	|None -> failwith "probleme transforme"

(*parcourir un chemin trouver le flot max sur ce chemin*)
let trouver_flot graphe chemin = 
	let flot_dispo_arc graphe id1 id2 = match (find_arc graphe id1 id2) with
		|Some(flot,capacite) -> capacite - flot
		|None -> failwith "problème dans trouver flot_dispo_arc"
	in
	(*rechercher le flot max du chemin*)
	let rec loop chemin accu = match chemin with
		|[] -> (-1)
		|[id] -> accu
		|id1::id2::rest -> if (flot_dispo_arc graphe id1 id2) < accu then loop (id2::rest) (flot_dispo_arc graphe id1 id2) else loop (id2::rest) accu
	in
	loop chemin max_int (*grand nombre qu'on suppose supérieur au flot dispo theorique max*)


(*parcourir le chemin, remplir les flots*)
let remplir_flot graphe chemin flotmax =
	(*ajouter flot max à chaque flot des arcs du chemin*)
	let remplacer graphe id1 id2 flotmax= match (find_arc graphe id1 id2) with 
		|Some(flot,capacite) -> add_arc graphe id1 id2 (flot+flotmax,capacite) (*on remplace l'arc avec la nouvelle valeur de flot*)
		|None -> failwith "erreur dans remplacer" 
	in
	let rec loop graphe chemin flotmax= match chemin with
		|[] -> failwith "problème dans la loop du remplir flot"
		|[id] -> graphe
		|id1::id2::rest -> loop (remplacer graphe id1 id2 flotmax) (id2::rest) flotmax (*nouveau graphe, reste du chemin et flot max en argument*)
	in
	loop graphe chemin flotmax


(*parcourir le chemin pour ajouter les arcs de retour*)
let remplir_retour graphe chemin flotmax =
	(*ajouter flot max à chaque flot des arcs du chemin*)
	let ajouter_retour graphe id1 id2 flotmax= match (find_arc graphe id2 id1) with 
		|Some(flot,capacite) -> add_arc graphe id2 id1 (flot,capacite + flotmax) (*on remplace l'arc avec la nouvelle valeur de flot*)
		|None -> add_arc graphe id2 id1 (0,flotmax)  (*on créé l'arc de retour s'il n'existe pas déjà*)
	in
	let rec loop graphe chemin flotmax= match chemin with
		|[] -> failwith "problème dans la loop du ajouter retour"
		|[id] -> graphe
		|id1::id2::rest -> loop (ajouter_retour graphe id1 id2 flotmax) (id2::rest) flotmax (*nouveau graphe, reste du chemin et flot max en argument*)
	in
	loop graphe chemin flotmax

let remplir_tout graphe chemin flotmax= remplir_retour (remplir_flot graphe chemin flotmax) chemin flotmax (*ceci appel les deux fonctions du dessus*)

let ford_fulkerson graphe origine arrive = 
	let rec loop graphe origine arrive =match trouver_flot graphe (transforme(find_path graphe origine arrive)) with (*on trouve un chemin et on le transforme (supprime le Some) puis on cherche le flot max sur ce chemin*)
		|(-1) -> graphe
		|flot -> loop (remplir_tout graphe (transforme(find_path graphe origine arrive)) flot) origine arrive

	in loop (init graphe) origine arrive







