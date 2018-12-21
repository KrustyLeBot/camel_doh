open Graph

type ford_graph = (int*int) graph

type chemin = id list

type path_list = chemin list

type forbidden = id list

val init: string graph -> ford_graph

val arc_dispo: Graph.id * (int * int ) -> bool (*a vérifier pour les paramètres d'entrée*)

val find_path: ford_graph -> Graph.id -> Graph.id -> Graph.id list option

val transforme: 'a option -> 'a

val trouver_flot: ford_graph -> chemin -> int

val remplir_flot: ford_graph -> chemin -> int -> ford_graph

val remplir_retour: ford_graph -> chemin -> int -> ford_graph

val remplir_tout: ford_graph -> chemin -> int -> ford_graph

val ford_fulkerson:  string graph -> Graph.id -> Graph.id -> ford_graph

