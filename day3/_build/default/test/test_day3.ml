open Hardcaml
open Day3

module Config = struct
  let length = 15
  let num = 4
end

module D3 = Day3(Config)

let create_sim () = 
  let module Sim = Cyclesim.With_interface(D3.I)(D3.O) in
  Sim.create D3.create
;;

let () =
  let open Bits in
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs._width := of_int ~width:64 2;
  inputs.length := of_int ~width:64 Config.length;
  inputs.num := of_int ~width:64 Config.num;

  let list = [
    "987654321111111";
    "811111111111119";
    "234234234234278";
    "818181911112111";
  ] in
  let array = Array.of_list list in

  let seq_len = 100 in

  for idx = 0 to Config.num - 1 do
    let s = array.(idx) in
    let seq_vals = Array.init seq_len (fun _ -> Bits.of_int ~width:4 0) in

    let pos = ref 0 in
    String.iter (fun c ->
      if !pos < seq_len then begin
        if Char.code c >= Char.code '0' && Char.code c <= Char.code '9' then
          seq_vals.(!pos) <- Bits.of_int ~width:4 ((Char.code c) - (Char.code '0'))
        else
          seq_vals.(!pos) <- Bits.of_int ~width:4 0;
        incr pos
      end
    ) s;

    List.iteri (fun i refcell ->
      refcell := seq_vals.(i)
    ) inputs.seq;

    (* Printf.printf "cycle %d: wrote \"%s\" (len=%d)\n%!" idx s (String.length s); *)

    while (to_bool !(outputs.get_next)) == false do
      Cyclesim.cycle sim;
      Printf.printf "ans=%d, state=%d\n!"
      (to_int !(outputs.ans))
      (to_int !(outputs.state))
    done;
  done;