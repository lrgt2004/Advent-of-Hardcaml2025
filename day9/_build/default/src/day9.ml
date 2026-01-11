open Base
open Hardcaml
open Signal

module type Config = sig
  val length : int
end

module Day9 (Config : Config) = struct
  module I = struct
    type 'a t = {
      clock : 'a [@bits 1];
      clear : 'a [@bits 1];
      length : 'a [@bits 64];
      x_axis : 'a [@bits 64];
      y_axis : 'a [@bits 64];
    }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      ans_part1 : 'a [@bits 64];
      ans_part2 : 'a [@bits 64];
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
      | S_outer_cycle
      | S_inner_cycle
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end
  let create (i : _ I.t) = begin
    let open Config in
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) r_sync in
    let outer_idx = Always.Variable.reg ~width:64 r_sync in
    let inner_idx = Always.Variable.reg ~width:64 r_sync in
    let write_idx = Always.Variable.reg ~width:64 r_sync in
    let write_enable = Always.Variable.wire ~default:gnd in
    let write_x = Always.Variable.reg ~width:64 r_sync in
    let write_y = Always.Variable.reg ~width:64 r_sync in
    let ans_part1 = Always.Variable.reg ~width:64 r_sync in
    let ans_part2 = Always.Variable.reg ~width:64 r_sync in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let _done = Always.Variable.reg ~width:1 r_sync in
    let read_done = Always.Variable.wire ~default:gnd in
    let x_axis = Ram.create
      ~collision_mode:Read_before_write
      ~size:length
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize outer_idx.value (Int.ceil_log2 length);
      };
      {
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize inner_idx.value (Int.ceil_log2 length);
      }|]
      ~write_ports:[|{
        write_clock = i.clock;
        write_enable = write_enable.value;
        write_address = Signal.uresize write_idx.value (Int.ceil_log2 length);
        write_data = write_x.value;
      }|]
      () in
    let y_axis = Ram.create
      ~collision_mode:Read_before_write
      ~size:length
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize outer_idx.value (Int.ceil_log2 length);
      };
      {
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize inner_idx.value (Int.ceil_log2 length);
      }|]
      ~write_ports:[|{
        write_clock = i.clock;
        write_enable = write_enable.value;
        write_address = Signal.uresize write_idx.value (Int.ceil_log2 length);
        write_data = write_y.value;
      }|]
      () in
    Always.(compile[
      sm.switch[
        (S_wait, [
          ans_part1 <-- gndn 64;
          ans_part2 <-- gndn 64;
          write_idx <-- gndn 64;
          sm.set_next S_read_read;
          read_done <-- vdd;
        ])
        ;
        (S_read_read, [
          if_ (write_idx.value ==: of_int ~width:64 length)
          [
            outer_idx <-- gndn 64;
            inner_idx <-- gndn 64;
            sm.set_next S_outer_cycle;
          ]
          [
            write_x <-- i.x_axis;
            write_y <-- i.y_axis;
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
        (S_outer_cycle, [
          if_ (outer_idx.value ==: of_int ~width:64 length)
          [
            sm.set_next S_done;
          ]
          [
            inner_idx <-- gndn 64;
            sm.set_next S_inner_cycle;
          ];
        ])
        ;
        (S_inner_cycle, [
          inner_idx <-- inner_idx.value +: vddn 64;
          when_ ((x_axis.(0) <=: x_axis.(1)) &: (y_axis.(0) <=: y_axis.(1))) 
          [(*outer左上/inner右下*)
            when_ (Signal.uresize ((x_axis.(1) -: x_axis.(0) +: vddn 64) *: (y_axis.(1) -: y_axis.(0) +: vddn 64)) 64 >: ans_part1.value)
            [
              ans_part1 <-- Signal.uresize ((x_axis.(1) -: x_axis.(0) +: vddn 64) *: (y_axis.(1) -: y_axis.(0) +: vddn 64)) 64;
            ];
          ];
          when_ ((x_axis.(0) <=: x_axis.(1)) &: (y_axis.(0) >: y_axis.(1))) 
          [(*outer左下/inner右上*)
            when_ (Signal.uresize ((x_axis.(1) -: x_axis.(0) +: vddn 64) *: (y_axis.(0) -: y_axis.(1) +: vddn 64)) 64 >: ans_part1.value)
            [
              ans_part1 <-- Signal.uresize ((x_axis.(1) -: x_axis.(0) +: vddn 64) *: (y_axis.(0) -: y_axis.(1) +: vddn 64)) 64;
            ]
          ];
          when_ ((x_axis.(0) >: x_axis.(1)) &: (y_axis.(0) <=: y_axis.(1))) 
          [(*outer右上/inner左下*)
            when_ (Signal.uresize ((x_axis.(0) -: x_axis.(1) +: vddn 64) *: (y_axis.(1) -: y_axis.(0) +: vddn 64)) 64 >: ans_part1.value)
            [
              ans_part1 <-- Signal.uresize ((x_axis.(0) -: x_axis.(1) +: vddn 64) *: (y_axis.(1) -: y_axis.(0) +: vddn 64)) 64;
            ];
          ];
          when_ ((x_axis.(0) >: x_axis.(1)) &: (y_axis.(0) >: y_axis.(1))) 
          [(*outer右下/inner左上*)
            when_ (Signal.uresize ((x_axis.(0) -: x_axis.(1) +: vddn 64) *: (y_axis.(0) -: y_axis.(1) +: vddn 64)) 64 >: ans_part1.value)
            [
              ans_part1 <-- Signal.uresize ((x_axis.(0) -: x_axis.(1) +: vddn 64) *: (y_axis.(0) -: y_axis.(1) +: vddn 64)) 64;
            ];
          ];
          when_ (inner_idx.value ==: of_int ~width:64 length)
          [
            outer_idx <-- outer_idx.value +: vddn 64;
            sm.set_next S_outer_cycle;
          ]
        ])
        ;
        (S_done, [
          _done <-- vdd;
        ])
        ;
      ]
    ]);
    {
      O.ans_part1 = ans_part1.value;
      ans_part2 = ans_part2.value;
      read_done = read_done.value;
      _done = _done.value;
    }
  end
end