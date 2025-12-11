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

  List.iteri (fun idx ang ->
    let string = array.(idx) in
    let angle = String.sub string 1 (String.length string - 1) in
    ang := Signal.of_int ~width:32 (int_of_string angle);
  ) inputs.rotations;

  Cyclesim.cycle sim;
