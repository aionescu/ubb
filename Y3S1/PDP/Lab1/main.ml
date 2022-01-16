(*
  3. Summation with fixed structure of inputs
  We have to keep the values of some integer variables. Some of them are primary variables; they represent input data.
  The others are secondary variables, and represent aggregations of some other variables. In our case, each secondary
  variable is a sum of some input variables. The inputs may be primary or secondary variables. However, we assume that
  the relations do not form cycles.

  At runtime, we get notifications of value changes for the primary variable. Processing a notification must atomically
  update the primary variable, as well as any secondary variable depending, directly or indirectly, on it. The updating
  shall not re-compute the sums; instead, you must use the difference between the old value and the new value of the
  primary variable.

  From time to time, as well as at the end, a consistency check shall be performed. It shall verify that all the secondary
  variables are indeed the sums of their inputs, as specified.
*)

type node = {
  id: string;
  mutex: Mutex.t;
  mutable value: int;
  mutable deps: node list;
  mutable revdeps: node list;
}

let new_node id = { id; mutex = Mutex.create (); value = 0; deps = []; revdeps = [] }

(* Creates a new graph with all deps/revdeps set up, but no values initialized. *)
let build_graph primary secondary =
  let g = Hashtbl.create (List.length primary + List.length secondary) in

  (* Add all nodes to graph *)
  primary |> List.iter (fun (i, _) -> Hashtbl.add g i (new_node i));
  secondary |> List.iter (fun (i, _) -> Hashtbl.add g i (new_node i));

  (* Initialize deps & revdeps *)
  secondary |> List.iter (fun (i, deps) ->
    let node = Hashtbl.find g i in
    let deps = List.map (Hashtbl.find g) deps in

    node.deps <- deps;
    deps |> List.iter (fun d -> d.revdeps <- node :: d.revdeps)
  );

  g

let print_graph g =
  Hashtbl.to_seq_values g
  |> List.of_seq
  |> List.sort (fun a b -> compare a.id b.id)
  |> List.iter (fun n -> Printf.printf "%s: %d\n" n.id n.value)

let update_single v n =
  Mutex.lock n.mutex;
  n.value <- n.value + v;
  Mutex.unlock n.mutex

let rec update_rec v n =
  update_single v n;
  List.iter (update_rec v) n.revdeps

let check_sum l = List.fold_right (fun a s ->
  match a, s with
  | Some a, Some s -> Some (a + s)
  | _ -> None
) l (Some 0)

let rec check_val n =
  match n.deps with
  | [] -> Some n.value
  | deps -> check_sum @@ List.map check_val deps

let check_consistency g =
  Hashtbl.to_seq_values g
  |> List.of_seq
  |> List.for_all (fun n -> Option.is_some @@ check_val n)

let rand_int a b = Random.int (b + 1 - a) + a
let rand_float a b = Random.float (b -. a) +. a
let rand_item l = List.nth l @@ Random.int @@ List.length l

let run_thread primary g count ix =
  for iter = 1 to count do
    let v = rand_int (-5) 10 in
    let delay = rand_float 0.5 1. in
    let node = Hashtbl.find g (fst @@ rand_item primary) in

    update_rec v node;
    Thread.delay delay;

    Printf.printf "Thread #%d updated \"%s\" (%d/%d) with %d after %fs\n" ix node.id iter count v delay;
    flush stdout
  done

let read_file path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let () =
  Random.self_init ();

  let primary, secondary = Parser.read_graph "Graph.txt" in

  (* Build graph and propagate values *)
  let g = build_graph primary secondary in
  primary |> List.iter (fun (i, v) -> update_rec v @@ Hashtbl.find g i);

  (* Run threads *)
  List.init (rand_int 10 15) (fun i -> Thread.create (run_thread primary g @@ rand_int 5 10) i)
  |> List.iter Thread.join;

  (* Print stats *)
  Printf.printf "\nFinal graph:\n";
  print_graph g;

  Printf.printf "\nConsistent: %b\n" @@ check_consistency g
