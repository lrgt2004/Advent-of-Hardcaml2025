open Hardcaml
open Day9
open List

module Config = struct
  let length = 8
end

module D9 = Day9(Config)

let create_sim () = 
  let module Sim = Cyclesim.With_interface(D9.I)(D9.O) in
  Sim.create D9.create
;;

let () = begin
  let open Bits in
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.length := of_int ~width:64 Config.length;
  let axis = [
    "7,1";
    "11,1";
    "11,7";
    "9,7";
    "9,5";
    "2,5";
    "2,3";
    "7,3";
  ] in
  let (x_axis, y_axis) = 
  List.fold_left (fun (x_axis, y_axis) range ->
    match String.split_on_char ',' range with
    | [x; y] ->
        let x_int = of_int ~width:64 (int_of_string x) in
        let y_int = of_int ~width:64 (int_of_string y) in
        (x_int :: x_axis, y_int :: y_axis)
    | _ -> (x_axis, y_axis)
  ) ([], []) axis in
  for idx = 0 to Config.length - 1 do
    inputs.x_axis := List.nth x_axis idx;
    inputs.y_axis := List.nth y_axis idx;
    Cyclesim.cycle sim;
    while(to_bool !(outputs.read_done)) == false do
      Cyclesim.cycle sim;
    done;
  done;
  while(to_bool !(outputs._done)) == false do
    Cyclesim.cycle sim;
  done;
  Printf.printf "Maximized Rectangle Size = %d\n%!" (to_int !(outputs.ans_part1));
  (* Printf.printf "Total fresh ingredient num = %d\n%!" (to_int !(outputs.ans_part2)); *)
end