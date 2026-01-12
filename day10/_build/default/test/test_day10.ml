open Hardcaml
open Day10

module Config = struct
  let max_press_length = 20
end

module D10 = Day10(Config)

let create_sim () = 
  let module Sim = Cyclesim.With_interface(D10.I)(D10.O) in
  Sim.create D10.create
;;

let () = begin
  let open Bits in
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let lights = [
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}";
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}";
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}";
  ] in
  let trim (s : string) : string =
    let open String in
    let l = length s in
    let i = ref 0 in
    while !i < l && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
    let j = ref (l - 1) in
    while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '\r' || s.[!j] = '\n') do decr j done;
    if !i > !j then "" else sub s !i (!j - !i + 1)
  in
  let extract_bracket s =
    try
      let l = String.index s '[' in
      let r = String.index_from s (l+1) ']' in
      Some (String.sub s (l+1) (r - l - 1))
    with _ -> None
  in
  let extract_paren_groups s =
    let rec aux i acc =
      try
        let l = String.index_from s i '(' in
        let r = String.index_from s (l+1) ')' in
        let inner = String.sub s (l+1) (r - l - 1) in
        aux (r+1) (inner :: acc)
      with Not_found -> List.rev acc
    in
    aux 0 []
  in
  let parse_indices inner =
    let inner = trim inner in
    if inner = "" then [] else
    inner
    |> String.split_on_char ','
    |> List.map trim
    |> List.filter (fun x -> x <> "")
    |> List.map int_of_string
  in
  let pattern_to_int_and_counts pat =
    let n = String.length pat in
    let acc = ref 0 in
    for i = 0 to n - 1 do
      let ch = pat.[i] in
      acc := (!acc lsl 1) + (if ch = '#' then 1 else 0)
    done;
    (!acc, n)
  in
  let indices_to_mask width indices =
    List.fold_left (fun acc idx ->
      if idx < 0 || idx >= width then acc
      else
        let pos = width - 1 - idx in
        acc lor (1 lsl pos)
    ) 0 indices
  in
  let results =
    List.mapi (fun _ line ->
      let line = trim line in
      match extract_bracket line with
      | None ->
        (0, 0, [])
      | Some pat ->
        let (pat_int, width) = pattern_to_int_and_counts pat in
        let paren_groups = extract_paren_groups line in
        let masks_ints =
          paren_groups
          |> List.map parse_indices
          |> List.map (fun idxs -> indices_to_mask width idxs)
        in
        (width, pat_int, masks_ints)
    ) lights
  in
  for idx = 0 to List.length results - 1 do
    let (width, pat_int, masks_ints) = List.nth results idx in
    inputs.final_state := of_int ~width:64 pat_int;
    inputs.state_length := of_int ~width:64 width;
    inputs.two_press_length := (sll (of_int ~width:64 1) (List.length masks_ints));
    inputs.press_length := of_int ~width:64 (List.length masks_ints);
    for inner_idx = 0 to List.length masks_ints - 1 do
      inputs.press_state := of_int ~width:64 (List.nth masks_ints inner_idx);
      Cyclesim.cycle sim;
      while(to_bool !(outputs.read_done)) == false do
        Cyclesim.cycle sim;
      done;
    done;
    while(to_bool !(outputs._done)) == false do
      Cyclesim.cycle sim;
    done;
  done;
  Printf.printf "Fewest Total Presses = %d\n%!" (to_int !(outputs.ans_part1));
end