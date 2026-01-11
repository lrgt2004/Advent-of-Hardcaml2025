open Hardcaml
open Day5
open List

module Config = struct
  let range_length = 4
  let query_length = 6
end

module D5 = Day5(Config)

let create_sim () = 
  let module Sim = Cyclesim.With_interface(D5.I)(D5.O) in
  Sim.create D5.create
;;

let () = 
  let open Bits in
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs.range_length := of_int ~width:64 Config.range_length;
  inputs.query_length := of_int ~width:64 Config.query_length;
  let ranges = [
    "3-5";
    "10-14";
    "16-20";
    "12-18";
  ] in
  let querys = [
    1;
    5;
    8;
    11;
    17;
    32;
  ] in
  let (starts, ends) = 
  List.fold_left (fun (starts, ends) range ->
    match String.split_on_char '-' range with
    | [start; finish] ->
        let start_int = of_int ~width:64 (int_of_string start) in
        let finish_int = of_int ~width:64 (int_of_string finish) in
        (start_int :: starts, finish_int :: ends)
    | _ -> (starts, ends)
  ) ([], []) ranges in
  let ref_start = ref starts in
  let ref_end = ref ends in
  let split_n lst num = 
    let rec aux lst num acc =
    match lst, num with
    | [], _ | _, 0 -> (List.rev acc, lst)
    | x :: xs, _ -> aux xs (num - 1) (x :: acc)
    in aux lst num []
  in
  let chunk_size = 32 in 
  let num_chunks = (Config.range_length + chunk_size - 1) / chunk_size in
  for _ = 0 to num_chunks - 1 do
    let lower_chunk, _start = split_n !ref_start chunk_size in
    let upper_chunk, _end = split_n !ref_end chunk_size in
    List.iteri (fun idx refbit ->
      if idx<List.length lower_chunk then refbit := List.nth lower_chunk idx
      else refbit := of_int ~width:64 0
    ) inputs.ranges_lower;
    List.iteri (fun idx refbit ->
      if idx<List.length upper_chunk then refbit := List.nth upper_chunk idx
      else refbit := of_int ~width:64 0
    ) inputs.ranges_upper;
    Cyclesim.cycle sim;
    while(to_bool !(outputs.read_done)) == false do
      Cyclesim.cycle sim;
    done;
    ref_start := _start;
    ref_end := _end;
  done;
  Cyclesim.cycle sim;
  while(to_bool !(outputs.read_done)) == false do
    Cyclesim.cycle sim;
  done;
  for idx = 0 to Config.query_length - 1 do
    inputs.query := of_int ~width:64 (List.nth querys idx);
    (* Printf.printf "%d\n%!" (to_int !(inputs.query)); *)
    Cyclesim.cycle sim;
    while(to_bool !(outputs.read_done)) == false do
      Cyclesim.cycle sim;
    done;
  done;
  
  while(to_bool !(outputs._done)) == false do
    Cyclesim.cycle sim;
  done;
  Printf.printf "Fresh ingredient num = %d\n%!" (to_int !(outputs.ans_part1));
  Printf.printf "Total fresh ingredient num = %d\n%!" (to_int !(outputs.ans_part2));