open Hardcaml
open Day7

module Config = struct
  let length = 15
end

module D7 = Day7(Config)

let create_sim () =
  let module Sim = Cyclesim.With_interface(D7.I)(D7.O) in
  Sim.create D7.create
;;

let () = 
  let open Bits in
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs.length := of_int ~width:64 Config.length;
  let list = [
    ".......S.......";
    "...............";
    ".......^.......";
    "...............";
    "......^.^......";
    "...............";
    ".....^.^.^.....";
    "...............";
    "....^.^...^....";
    "...............";
    "...^.^...^.^...";
    "...............";
    "..^...^.....^..";
    "...............";
    ".^.^.^.^.^...^.";
    "...............";
  ] in
  let n = Config.length * (Config.length + 1) in
  let flatten_lines lines =
    let arr = Array.of_list lines in
    Array.init n (fun idx ->
      let r = idx / Config.length in
      let c = idx mod Config.length in
      arr.(r).[c]
    )
  in

  let flat = flatten_lines list in
  let chunk_size = 32 in
  let num_chunks = (n + chunk_size - 1) / chunk_size in

  for chunk_idx = 0 to num_chunks - 1 do
    let chunk_bits = Array.init chunk_size (fun i ->
      let bit_idx = chunk_idx * chunk_size + i in
      if bit_idx < n then
        match flat.(bit_idx) with
        | '^' -> vdd
        | '.' | 'S' -> gnd
        | _ -> failwith "Invalid character in map"
      else
        gnd
    ) in

    List.iteri (fun idx refbit ->
      refbit := chunk_bits.(idx)
    ) inputs.map;

    (* Printf.printf "Cycle %d: inputs.map = [ " chunk_idx;
    Array.iter (fun refbit ->
      let value = if refbit = vdd then "vdd" else "gnd" in
      Printf.printf "%s " value
    ) chunk_bits;
    Printf.printf "]\n%!"; *)

    Cyclesim.cycle sim;
    while (to_bool !(outputs.read_done)) == false do
      Cyclesim.cycle sim;
    done;
  done;
  while (to_bool !(outputs._done)) == false do
    Cyclesim.cycle sim;
  done;

  Printf.printf "Split time = %d\n%!" (to_int !(outputs.ans_part1));
  Printf.printf "Total timelines = %d\n%!" (to_int !(outputs.ans_part2));