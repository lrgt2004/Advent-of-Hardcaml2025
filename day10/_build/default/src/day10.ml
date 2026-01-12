open Base
open Hardcaml
open Signal

module type Config = sig
  (* val tot_length : int *)
  val max_press_length : int
end

module Day10 (Config : Config) = struct
  module I = struct
    type 'a t = {
      clock : 'a [@bits 1];
      clear : 'a [@bits 1];
      state_length : 'a [@bits 64];
      two_press_length : 'a [@bits 64];
      final_state : 'a [@bits 64];
      press_length : 'a [@bits 64];
      press_state : 'a [@bits 64];
    }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      ans_part1 : 'a [@bits 64];
      read_done : 'a [@bits 1];
      _done : 'a [@bits 1];
    }
    [@@deriving hardcaml]
  end
  module States = struct
    type t = 
      | S_wait
      | S_read_read
      | S_read_write
      | S_try
      | S_check
      | S_check_read
      | S_check_xor
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end
  let create (i : _ I.t) = begin
    let open Config in
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) r_sync in
    (* let tot = Always.Variable.reg ~width:64 r_sync in *)
    let outer_idx = Always.Variable.reg ~width:64 r_sync in
    let inner_idx = Always.Variable.reg ~width:64 r_sync in
    let write_idx = Always.Variable.reg ~width:64 r_sync in
    let write_enable = Always.Variable.wire ~default:gnd in
    let write_data = Always.Variable.reg ~width:64 r_sync in
    let check = Always.Variable.reg ~width:64 r_sync in
    let xor = Always.Variable.reg ~width:64 r_sync in
    let cnt = Always.Variable.reg ~width:64 r_sync in
    let min_cnt = Always.Variable.reg ~width:64 r_sync in
    let ans_part1 = Always.Variable.reg ~width:64 r_sync in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let _done = Always.Variable.wire ~default:gnd in
    let read_done = Always.Variable.wire ~default:gnd in
    let press = Ram.create
      ~collision_mode:Read_before_write
      ~size:max_press_length
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize inner_idx.value (Int.ceil_log2 max_press_length);
      }|]
      ~write_ports:[|{
        write_clock = i.clock;
          write_enable = write_enable.value;
          write_address = Signal.uresize write_idx.value (Int.ceil_log2 max_press_length);
          write_data = write_data.value;
      }|]
      () in
    Always.(compile[
      sm.switch[
        (S_wait, [
          (* tot <-- tot.value +: vddn 64; *)
          write_idx <-- gndn 64;
          sm.set_next S_read_read;
        ])
        ;
        (S_read_read, [
          if_ (write_idx.value ==: i.press_length)
          [
            outer_idx <-- gndn 64;
            min_cnt <-- of_int ~width:64 1000;
            sm.set_next S_try;
          ]
          [
            write_data <-- i.press_state;
            sm.set_next S_read_write;
          ];
        ])
        ;
        (S_read_write, [
          write_enable <-- vdd;
          write_idx <-- write_idx.value +: vddn 64;
          read_done <-- vdd;
          sm.set_next S_read_read;
        ])
        ;
        (S_try, [
          if_ (outer_idx.value ==: i.two_press_length)
          [
            ans_part1 <-- ans_part1.value +: min_cnt.value;
            sm.set_next S_done;
          ]
          [
            inner_idx <-- i.press_length -: vddn 64;
            check <-- gndn 64;
            cnt <-- gndn 64;
            xor <-- gndn 64;
            sm.set_next S_check;
          ];
        ])
        ;
        (S_check, [
          if_ (inner_idx.value ==: of_int ~width:64 (-1))
          [
            when_ (check.value ==: i.final_state)
            [
              when_ (cnt.value <: min_cnt.value)
              [min_cnt <-- cnt.value;]
            ];
            sm.set_next S_try;
            outer_idx <-- outer_idx.value +: vddn 64;
          ]
          [
            let flag =
              let rec loop i acc =
                if i = 64 then acc
                else
                  let eq = inner_idx.value ==: of_int ~width:64 i in
                  let bit = Signal.select outer_idx.value i i in
                  loop (i+1) (acc |: (eq &: bit))
              in
              loop 0 gnd
            in
            if_ (flag ==: vdd)
            [
              cnt <-- cnt.value +: vddn 64;
              sm.set_next S_check_read;
            ]
            [
              inner_idx <-- inner_idx.value -: vddn 64;
            ];
          ];
        ])
        ;
        (S_check_read, [
          xor <-- press.(0);
          sm.set_next S_check_xor;
        ])
        ;
        (S_check_xor, [
          check <-- check.value ^: xor.value;
          inner_idx <-- inner_idx.value -: vddn 64;
          sm.set_next S_check;
        ])
        ;
        (S_done, [
          _done <-- vdd;
          sm.set_next S_wait;
        ])
      ]
    ]);
    {
      O.ans_part1 = ans_part1.value;
      read_done = read_done.value;
      _done = _done.value;
    }
  end
end