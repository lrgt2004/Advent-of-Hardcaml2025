open Base
open Hardcaml
open Signal

module type Config = sig
  val length : int
end

module Day7 (Config : Config) = struct 
  module I = struct 
    type 'a t = {
      clock : 'a [@bits 1];
      clear : 'a [@bits 1];
      length : 'a [@bits 64];
      map : 'a list [@bits 1] [@length 32];
    }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      ans_part1 : 'a [@bits 64];
      ans_part2 : 'a [@bits 64];
      state : 'a [@bits 3];
      read_done : 'a [@bits 1];
      _done : 'a [@bits 1];
    }
    [@@deriving hardcaml]
  end

  module States = struct 
    type t = 
      | S_wait
      | S_read
      | S_split_read
      | S_split_write
      | S_wait_ram
      | S_count
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (i : _ I.t) = begin
    let open Config in
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) r_sync in
    let ans_part1 = Always.Variable.reg ~width:64 r_sync in
    let ans_part2 = Always.Variable.reg ~width:64 r_sync in
    let read_map_idx = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let read_count_idx = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let write_map_idx = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let write_count_idx = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let write_map_en = Always.Variable.wire ~default:gnd in
    let write_count_en = Always.Variable.wire ~default:gnd in
    let write_start_en = Always.Variable.wire ~default:gnd in
    let write_map = Always.Variable.wire ~default:gnd in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let _done = Always.Variable.reg ~width:1 r_sync in
    let read_done = Always.Variable.wire ~default:gnd in
    let split_do_write = Always.Variable.reg ~width:1 ~enable:vdd r_sync in
    let write_prev_data = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let write_cur_data  = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let write_nxt_data  = Always.Variable.reg ~width:64 ~enable:vdd r_sync in
    let ram_map = Ram.create
      ~collision_mode:Read_before_write
      ~size:(length*(length+1))
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize read_map_idx.value (Int.ceil_log2 (length*(length+1)));
      };|]
      ~write_ports:[|{
        write_clock = i.clock;
        write_enable = write_map_en.value;
        write_address = Signal.uresize write_map_idx.value (Int.ceil_log2 (length*(length+1)));
        write_data = write_map.value;
      }|] 
      () in

    let ram_count = Ram.create
      ~collision_mode:Write_before_read
      ~size:length
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize (read_count_idx.value -: vddn 64) (Int.ceil_log2 length);
      };
      {
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize read_count_idx.value (Int.ceil_log2 length);
      };
      {
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize (read_count_idx.value +: vddn 64) (Int.ceil_log2 length);
      };|]
      ~write_ports:[|
      {
        write_clock = i.clock;
        write_enable = write_count_en.value;
        write_address = Signal.uresize (write_count_idx.value -: vddn 64) (Int.ceil_log2 length);
        write_data = write_prev_data.value;
      };
      {
        write_clock = i.clock;
        write_enable = write_count_en.value;
        write_address = Signal.uresize write_count_idx.value (Int.ceil_log2 length);
        write_data = write_cur_data.value;
      };
      {
        write_clock = i.clock;
        write_enable = write_count_en.value;
        write_address = Signal.uresize (write_count_idx.value +: vddn 64) (Int.ceil_log2 length);
        write_data = write_nxt_data.value;
      };
      {
        write_clock = i.clock;
        write_enable = write_start_en.value;
        write_address = Signal.of_int ~width:(Int.ceil_log2 length) (length/2);
        write_data = vddn 64;
      };|]
      () in
    let prev = ram_count.(0) in
    let cur = ram_count.(1) in
    let nxt = ram_count.(2) in
    let cell = ram_map.(0) in
    Always.(compile[
      sm.switch[
        (S_wait, [
          ans_part1 <-- gndn 64;
          ans_part2 <-- gndn 64;
          read_map_idx <-- gndn 64;
          write_map_idx <-- gndn 64;
          read_done <-- gnd;
          _done <-- gnd;
          sm.set_next S_read;
        ])
        ;
        (S_read, [
          if_ (read_map_idx.value ==: Signal.of_int ~width:64 32) 
          [
            read_done <-- vdd;
            read_map_idx <-- gndn 64;
          ]
          [
            write_map_en <-- vdd;
            write_map <-- mux (Signal.uresize read_map_idx.value 6) i.map;
            write_map_idx <-- write_map_idx.value +: vddn 64;
            read_map_idx <-- read_map_idx.value +: vddn 64;
          ];
          when_ (write_map_idx.value ==: Signal.of_int ~width:64 (length*(length+1)))
          [
            read_done <-- vdd;
            read_count_idx <-- gndn 64;
            read_map_idx <-- gndn 64;
            write_count_idx <-- Signal.of_int ~width:64 (-1);
            write_start_en <-- vdd;
            sm.set_next S_split_read;
          ];
        ])
        ;
        (S_split_read, [
          if_ ((cell ==: vdd) &: (cur <>: gndn 64))
          [
            write_prev_data <-- prev +: cur;
            write_cur_data  <-- gndn 64;
            write_nxt_data  <-- cur +: nxt;
            split_do_write <-- vdd;
            ans_part1 <-- ans_part1.value +: vddn 64;
            sm.set_next S_split_write;
          ]
          [
            write_prev_data <-- prev;
            write_cur_data  <-- cur;
            write_nxt_data  <-- nxt;
            split_do_write <-- gnd;
            sm.set_next S_split_write;
          ];
          write_count_idx <-- write_count_idx.value +: vddn 64;
          when_ (read_map_idx.value >=: Signal.of_int ~width:64 (length*(length+1)-1))
          [
            read_count_idx <-- gndn 64;
            sm.set_next S_count;
          ];
        ])
        ;
        (S_split_write, [
          write_count_en <-- split_do_write.value;
          read_map_idx <-- read_map_idx.value +: vddn 64;
          split_do_write <-- gnd;
          if_ (read_count_idx.value >=: Signal.of_int ~width:64 (length - 1))
          [
            read_count_idx <-- gndn 64;
            write_count_idx <-- Signal.of_int ~width:64 (-1);
          ]
          [
            read_count_idx <-- read_count_idx.value +: vddn 64;
          ];
          sm.set_next S_wait_ram;
        ])
        ;
        (S_wait_ram, [
          sm.set_next S_split_read;
        ])
        ;
        (S_count, [
          if_ (read_count_idx.value ==: Signal.of_int ~width:64 length)
          [
            sm.set_next S_done;
          ]
          [
            ans_part2 <-- ans_part2.value +: cur;
            read_count_idx <-- read_count_idx.value +: vddn 64;
          ];
        ])
        ;
        (S_done, [
          _done <-- vdd;
        ])
        ;
      ]
    ]);
    {O.ans_part1 = ans_part1.value;
    ans_part2 = ans_part2.value;
    state = sm.current;
    read_done = read_done.value;
    _done = _done.value;
    }
  end
end