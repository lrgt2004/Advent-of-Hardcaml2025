open Hardcaml
open Day1

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
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  
  List.iteri (fun idx dirc ->
    let string = array.(idx) in
    let direction = String.get string 0 in
    
    if direction == 'L' then begin
      dirc := gnd;
    end else if direction = 'R' then begin
      dirc := vdd;
    end;
  ) inputs.directions;

  let rotations = List.mapi (fun idx item ->
    let number_str = String.sub item 1 (String.length item - 1) in
    int_of_string number_str
  ) list in

  inputs.rotations = List.map (Signal.of_int ~width:32) rotations;

  Cyclesim.reset sim;
  for _ = 1 to Config.length*100 do
    Cyclesim.cycle sim;
  done;

  let _ans1 = !(outputs.ans_part1) in
  let _ans2 = !(outputs.ans_part2) in
  let ans1 = Bits.to_int _ans1 in
  let ans2 = Bits.to_int _ans2 in
  Printf.printf "Password = %d\n%!" ans1;
  Printf.printf "True password = %d\n%!" ans2;