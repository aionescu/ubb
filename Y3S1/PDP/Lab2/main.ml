(*
  Create two threads, a producer and a consumer, with the producer feeding the consumer.

  Requirement: Compute the scalar product of two vectors.

  Create two threads. The first thread (producer) will compute the products of pairs of
  elements - one from each vector - and will feed the second thread. The second thread (consumer)
  will sum up the products computed by the first one. The two threads will behind synchronized
  with a condition variable and a mutex. The consumer will be cleared to use each product as
  soon as it is computed by the producer thread.
*)

let next: int option ref = ref None
let m = Mutex.create ()
let c = Condition.create ()

let producer pairs =
  let send_next v =
    Mutex.lock m;
    next := v;
    Condition.signal c
  in

  pairs |> List.iter (fun (a, b) -> send_next @@ Some (a * b));
  send_next None

let consumer () =
  let sum = ref 0 in

  Mutex.lock m;
  while !next <> None do
    Condition.wait c m;
    sum := !sum + Option.get !next
  done;

  Printf.printf "Dot-product is: %d\n" !sum;
  Mutex.unlock m

let read_file path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let () =
  let vs = Parser.read_vectors "Vectors.txt" in

  let consumer = Thread.create consumer () in
  let producer = Thread.create producer vs in

  Thread.join consumer;
  Thread.join producer
