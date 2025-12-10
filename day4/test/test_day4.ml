open Hardcaml
open Day4

module Config = struct
  let row_size = 10
  let col_size = 10
end

module D4 = Day4(Config)

let create_sim () = 
  let module Sim = Cyclesim.With_interface(D4.I)(D4.O) in 
  Sim.create D4.create
;;
let () =
  let open Bits in
  let n = Config.row_size * Config.col_size in

  let test_lines = [
"..@@.@@@@.";
"@@@.@.@.@@";
"@@@@@.@.@@";
"@.@@@@..@.";
"@@.@@@@.@@";
".@@@@@@@.@";
".@.@.@.@@@";
"@.@@@.@@@@";
".@@@@@@@@.";
"@.@.@@@.@.";
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
  
  List.iteri
    (fun idx refbit ->
       let bit =
         match flat.(idx) with
         | '@' -> vdd
         | '.' -> gnd
         | _ -> failwith "Invalid character in map"
       in
       refbit := bit
    )
    inputs.map;
  
  inputs.col_size := of_int ~width:32 Config.col_size;
  inputs.row_size := of_int ~width:32 Config.row_size;

  Cyclesim.reset sim;
  for _ = 1 to (Config.col_size) * n do
    Cyclesim.cycle sim;
  done;

  let _ans1 = !(outputs.ans_part1) in
  let _ans2 = !(outputs.ans_part2) in
  let ans1 = Bits.to_int _ans1 in
  let ans2 = Bits.to_int _ans2 in
  Printf.printf "Accessible paper rolls (count) = %d\n%!" ans1;
  Printf.printf "Largest area of connected paper rolls = %d\n%!" ans2