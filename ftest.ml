open Graph

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile (pas le bon nombre d arguments)\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

 (*
  let f str = int_of_string str in (*à remplacer par une fonc qui prend un string et retourne un int int_of_string str *)

  let graphe = map graph f in(* on remplace les string des labels des arcs par des int*)
  *)

  let graphe2 = Flow_network.ford_fulkerson graph "0" "5" in

  (* Rewrite the graph that has been read. *)
  let () = Gfile.export outfile (Gfile.conversion(graphe2)) in

  ()
