open Hardcaml
open Base
open Signal

module type Config = sig
  val length : int
end

module Day1 (Config : Config) = struct
  open Config
  module I = struct
    type 'a t = {
      clock : 'a [@bits 1];
      clear : 'a [@bits 1];
      length : 'a [@bits 32];
      directions : 'a list [@length 32] [@bits 1]; (*gnd for left, vdd for right*)
      angles : 'a list [@length 32] [@bits 32];
    }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      ans_part1 : 'a [@bits 32];
      ans_part2 : 'a [@bits 32];
      state : 'a [@bits 3];
      read_done : 'a [@bits 1];
      _done : 'a [@bits 1];
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | S_wait
      | S_wait_for_read
      | S_read_list
      | S_read
      | S_mod
      | S_rotate
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end
  let create (i : _ I.t) = begin
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) r_sync in
    let angle = Always.Variable.reg ~width:32 r_sync in
    let direction = Always.Variable.reg ~width:1 r_sync in (*gnd for left, vdd for right*)
    let read_idx = Always.Variable.reg ~width:32 r_sync in
    let write_idx = Always.Variable.reg ~width:32 r_sync in
    let remaining = Always.Variable.reg ~width:32 r_sync in
    let ans_part1 = Always.Variable.reg ~width:32 r_sync in
    let ans_part2 = Always.Variable.reg ~width:32 r_sync in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let hundred = Signal.of_int ~width:32 100 in
    let write_en = Always.Variable.wire ~default:gnd in
    let write_dirc = Always.Variable.reg ~width:1 r_sync in
    let write_angle = Always.Variable.reg ~width:32 r_sync in
    let ram_direction = 
      (Ram.create
      ~collision_mode:Write_before_read
      ~size:length
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize read_idx.value (Int.ceil_log2 length);
      }|]
      ~write_ports:[|{
        write_clock = i.clock;
        write_enable = write_en.value;
        write_address = Signal.uresize write_idx.value (Int.ceil_log2 length);
        write_data = write_dirc.value;
      }|]
      ()) in
    let ram_angle = 
      (Ram.create
      ~collision_mode:Write_before_read
      ~size:(length)
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize read_idx.value (Int.ceil_log2 (length));
      }|]
      ~write_ports:[|{
        write_clock = i.clock;
        write_enable = write_en.value;
        write_address = Signal.uresize write_idx.value (Int.ceil_log2 (length));
        write_data = write_angle.value;
      }|]
      ()) in
    let _done = Always.Variable.reg ~width:1 r_sync in
    let read_done = Always.Variable.wire ~default:gnd in
    Always.(
      compile[ sm.switch[
      (S_wait,
      [
        read_idx <-- gndn 32;
        write_idx <-- Signal.of_int ~width:32 (-1);
        angle <-- Signal.of_int ~width:32 50;
        remaining <-- gndn 32;
        ans_part1 <-- gndn 32;
        ans_part2 <-- gndn 32;
        _done <-- gnd;
        direction <-- gnd;
        sm.set_next S_read_list;
      ])
      ;
      (S_wait_for_read,
      [
        when_ (angle.value ==: gndn 32) [ ans_part1 <-- ans_part1.value +: vddn 32;];
        sm.set_next S_read;
      ]);
      (S_read_list,
      [
        if_ (read_idx.value ==: Signal.of_int ~width:32 32)
        [
          read_done <-- vdd;
          read_idx <-- gndn 32;
        ]
        [
          write_en <-- vdd;
          write_dirc <-- mux read_idx.value i.directions;
          write_angle <-- mux read_idx.value i.angles;
          write_idx <-- write_idx.value +: vddn 32;
          read_idx <-- read_idx.value +: vddn 32;
        ];
        if_ (write_idx.value ==: i.length)
        [
          read_done <-- vdd;
          read_idx <-- gndn 32;
          sm.set_next S_wait_for_read;
        ]
        [];
      ])
      ;
      (S_read,
      [
        if_ (read_idx.value ==: Signal.of_int ~width:32 length)
        [
          sm.set_next S_done;
        ]
        [
          remaining <-- ram_angle.(0);
          direction <-- ram_direction.(0);
          read_idx <-- read_idx.value +: vddn 32;
          if_ (ram_angle.(0) >=: hundred)
          [
            sm.set_next S_mod;
          ]
          [
            sm.set_next S_rotate;
          ];
        ];
      ])
      ;
      (S_mod,
      [
        if_ (remaining.value >=: hundred)
        [
          remaining <-- remaining.value -: hundred;
          ans_part2 <-- ans_part2.value +: vddn 32;
        ]
        [
          sm.set_next S_rotate;
        ];
      ])
      ;
      (S_rotate,
      [
        remaining <-- remaining.value -: vddn 32;
        if_ (remaining.value ==: gndn 32)
        [
          sm.set_next S_wait_for_read;
        ]
        [
          if_ (direction.value ==: gnd)
          [ (*left*)
            if_ (angle.value ==: gndn 32)
            [
              angle <-- Signal.of_int ~width:32 99;
            ]
            [
              angle <-- angle.value -: vddn 32;
            ];
          ]
          [ (*right*)
            if_ (angle.value ==: Signal.of_int ~width:32 99)
            [
              angle <-- gndn 32;
            ]
            [
              angle <-- angle.value +: vddn 32;
            ];
          ];
          when_ (angle.value ==: gndn 32)
          [
            ans_part2 <-- ans_part2.value +: vddn 32;
          ];
        ];
      ])
      ;
      (S_done,
      [
        _done <-- vdd;
      ])
    ]
    ]);
    { O.ans_part1 = ans_part1.value;
      ans_part2 = ans_part2.value;
      state = sm.current;
      read_done = read_done.value;
      _done = _done.value;
    }
  end
end