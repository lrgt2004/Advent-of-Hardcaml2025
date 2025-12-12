open Hardcaml
open Day1

(* exception Break *)

module Config = struct 
  let length = 10
end

module D1 = Day1(Config)

let create_sim () = 
  let module Sim = Cyclesim.With_interface(D1.I)(D1.O) in
  Sim.create D1.create
;;

let () = 
  let open Bits in
  let list = [
    "L68";
    "L30";
    "R48";
    "L5";
    "R60";
    "L55";
    "L1";
    "L99";
    "R14";
    "L82";
  ] in
  let array = Array.of_list list in
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let chunk_size = 32 in
  let num_chunks = (Config.length + chunk_size - 1) / chunk_size in

  inputs.length := of_int ~width:32 Config.length;

  for chunk_idx = 0 to num_chunks - 1 do
    let chunk_dircs = Array.init chunk_size (fun i ->
      let bit_idx = chunk_idx * chunk_size + i in
      if bit_idx < Config.length then
        match (String.get array.(bit_idx) 0) with
        | 'L' -> gnd
        | 'R' -> vdd
        | _ -> failwith "Invalid direction"
      else gnd
    ) in

    List.iteri (fun idx refbit ->
      refbit := chunk_dircs.(idx)
    ) inputs.directions;

    (* Printf.printf "Cycle %d: inputs.directions = [ " chunk_idx;
    Array.iter (fun refbit ->
      let value = if refbit = vdd then "R" else "L" in
      Printf.printf "%s " value
    ) chunk_dircs;
    Printf.printf "]\n"; *)

    let chunk_angles = Array.init chunk_size (fun i ->
      let bit_idx = chunk_idx * chunk_size + i in
      if bit_idx < Config.length then
        let str = array.(bit_idx) in
        let number_str = String.sub str 1 (String.length str - 1) in
        Bits.of_int ~width:32 (int_of_string number_str)
      else
        Bits.of_int ~width:32 0
    )
    in
    List.iteri (fun idx refbit ->
      refbit := chunk_angles.(idx)
    ) inputs.angles;

    (* Printf.printf "Cycle %d: inputs.angles = [ " chunk_idx;
    Array.iter (fun refbit ->
      let value = to_string refbit in
      Printf.printf "%s " value
    ) chunk_angles;
    Printf.printf "]\n%!"; *)

    Cyclesim.cycle sim;
    while (to_bool !(outputs.read_done)) == false do
      Cyclesim.cycle sim;
    done;
  done;
    while (to_bool !(outputs._done)) == false do
      Cyclesim.cycle sim;
    done;
    let _ans1 = !(outputs.ans_part1) in
    let _ans2 = !(outputs.ans_part2) in
    let ans1 = Bits.to_int _ans1 in
    let ans2 = Bits.to_int _ans2 in
    Printf.printf "Password = %d\n%!" ans1;
    Printf.printf "True password = %d\n%!" ans2;