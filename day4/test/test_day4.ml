open Hardcaml
open Day4

module Config = struct
  let row_size = 20
  let col_size = 20
end

exception Break

module D4 = Day4(Config)

let create_sim () = 
  let module Sim = Cyclesim.With_interface(D4.I)(D4.O) in 
  Sim.create D4.create
;;

let () =
  let open Bits in
  let n = Config.row_size * Config.col_size in

  let test_lines = [
    "@.@..@..@.@..@.@.@@@";
    "@@@..@..@..@@..@.@.@";
    "@.@@.@.@.@@@..@.@..@";
    "@.@.@@.@@@.@..@.@@.@";
    "@.@.@@.@.@.@.@.@@@.@";
    ".@..@@..@.@@@.@.@.@.";
    "@.@..@.@..@.@..@..@.";
    "@@.@.@.@@@..@.@@.@..";
    ".@.@..@.@.@.@..@.@@.";
    "@@@.@@.@@..@.@@.@@..";
    "@.@.@.@.@@@.@.@@.@.@";
    "..@@@.@..@.@.@@@.@..";
    ".@.@..@@.@.@.@@.@@..";
    "@.@.@@.@.@.@@@.@.@..";
    "@.@@.@@.@.@..@.@.@..";
    "@@.@@..@@@.@..@.@..@";
    "@..@..@.@@.@.@.@.@.@";
    "@.@@..@@.@.@@@.@.@@@";
    "@.@..@..@..@@@..@..@";
    "@.@@.@.@@.@@..@..@@@";
  ] in

  let flatten_lines lines =
    let arr = Array.of_list lines in
    Array.init n (fun idx ->
      let r = idx / Config.col_size in
      let c = idx mod Config.col_size in
      arr.(r).[c]
    )
  in

  let flat = flatten_lines test_lines in

  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  inputs.row_size := of_int ~width:32 Config.row_size;
  inputs.col_size := of_int ~width:32 Config.col_size;

  let chunk_size = 32 in
  let num_chunks = (n + chunk_size - 1) / chunk_size in

  for chunk_idx = 0 to num_chunks - 1 do
    let chunk_bits = Array.init chunk_size (fun i ->
      let bit_idx = chunk_idx * chunk_size + i in
      if bit_idx < n then
        match flat.(bit_idx) with
        | '@' -> vdd
        | '.' -> gnd
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
    Printf.printf "]\n"; *)

    (* Printf.printf "state=%d, read_idx=%d, write_idx=%d\n" (to_int !(outputs.state)) (to_int !(outputs.read_idx)) (to_int !(outputs.write_idx)); *)
    while (to_bool !(outputs.read_done)) == false do
      Cyclesim.cycle sim;
      (* Printf.printf "state=%d, read_idx=%d, write_idx=%d, cell=%d\n" 
      (to_int !(outputs.state))
      (to_int !(outputs.read_idx)) 
      (to_int !(outputs.write_idx))
      (to_int !(outputs.cell)); *)
    done;
  done;
  try
    for _ = 1 to (Config.col_size * n) do
      Cyclesim.cycle sim;
      (* Printf.printf "result=%d, state=%d, read_idx=%d, write_idx=%d, x=%d, y=%d, cell=%d\n" 
      (to_int !(outputs.ans_part1))
      (to_int !(outputs.state))
      (to_int !(outputs.read_idx))
      (to_int !(outputs.write_idx))
      (to_int !(outputs.x))
      (to_int !(outputs.y))
      (to_int !(outputs.cell)); *)
      if (to_bool !(outputs._done)) == true then
        raise Break;
    done;
  with Break ->
    let _ans1 = !(outputs.ans_part1) in
    let _ans2 = !(outputs.ans_part2) in
    let ans1 = Bits.to_int _ans1 in
    let ans2 = Bits.to_int _ans2 in

    Printf.printf "Accessible paper rolls (count) = %d\n%!" ans1;
    Printf.printf "Largest area of connected paper rolls = %d\n%!" ans2;
