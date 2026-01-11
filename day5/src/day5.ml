open Base
open Hardcaml
open Signal

module type Config = sig 
  val range_length : int
  val query_length : int
end

module Day5 (Config : Config) = struct
  module I = struct
    type 'a t = {
      clock : 'a [@bits 1];
      clear : 'a [@bits 1];
      range_length : 'a [@bits 64];
      query_length : 'a [@bits 64];
      ranges_upper : 'a list [@length 32] [@bits 64];
      ranges_lower : 'a list [@length 32] [@bits 64];
      query : 'a [@bits 64];
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
      | S_sort
      | S_compare_read
      | S_compare
      | S_swap_1
      | S_swap_2
      | S_part1
      | S_add_ans
      | S_part2
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (i : _ I.t) = begin
    let open Config in
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) r_sync in
    let read_upper = Always.Variable.reg ~width:64 r_sync in
    let read_lower = Always.Variable.reg ~width:64 r_sync in
    let write_upper = Always.Variable.reg ~width:64 r_sync in
    let write_upper_enable = Always.Variable.wire ~default:gnd in
    let write_upper_data = Always.Variable.reg ~width:64 r_sync in
    let write_lower = Always.Variable.reg ~width:64 r_sync in
    let write_lower_enable = Always.Variable.wire ~default:gnd in
    let write_lower_data = Always.Variable.reg ~width:64 r_sync in
    let mn_lower = Always.Variable.reg ~width:64 r_sync in
    let mn_upper = Always.Variable.reg ~width:64 r_sync in
    let lower_swap = Always.Variable.reg ~width:64 r_sync in
    let upper_swap = Always.Variable.reg ~width:64 r_sync in
    let idx = Always.Variable.reg ~width:64 r_sync in (*选择排序的外层循环变量*)
    let mn_idx = Always.Variable.reg ~width:64 r_sync in 
    let ans_part1 = Always.Variable.reg ~width:64 r_sync in
    let query_cnt = Always.Variable.reg ~width:64 r_sync in
    let ans_part2 = Always.Variable.reg ~width:64 r_sync in
    let cur_tail = Always.Variable.reg ~width:64 r_sync in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let _done = Always.Variable.reg ~width:1 r_sync in
    let read_done = Always.Variable.wire ~default:gnd in
    let upper = Ram.create
      ~collision_mode:Read_before_write
      ~size:range_length
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize read_upper.value (Int.ceil_log2 range_length);
      }|]
      ~write_ports:[|{
        write_clock = i.clock;
        write_enable = write_upper_enable.value;
        write_address = Signal.uresize write_upper.value (Int.ceil_log2 range_length);
        write_data = write_upper_data.value;
      }|]
      () in
    let lower = Ram.create
      ~collision_mode:Read_before_write
      ~size:range_length
      ~read_ports:[|{
        read_clock = i.clock;
        read_enable = vdd;
        read_address = Signal.uresize read_lower.value (Int.ceil_log2 range_length);
      }|]
      ~write_ports:[|{
        write_clock = i.clock;
        write_enable = write_lower_enable.value;
        write_address = Signal.uresize write_lower.value (Int.ceil_log2 range_length);
        write_data = write_lower_data.value;
      }|]
      () in
    Always.(compile[
      sm.switch[
        (S_wait, [
          read_upper <-- gndn 64;
          write_upper <-- gndn 64;
          read_lower <-- gndn 64;
          write_lower <-- gndn 64;
          read_done <-- gnd;
          _done <-- gnd;
          sm.set_next S_read_read;
        ])
        ;
        (S_read_read, [
          if_ (read_upper.value ==: Signal.of_int ~width:64 32) 
          [
            read_done <-- vdd;
            read_upper <-- gndn 64;
            read_lower <-- gndn 64;
          ]
          [
            write_lower_data <-- mux (Signal.uresize read_lower.value 6) i.ranges_lower;
            write_upper_data <-- mux (Signal.uresize read_upper.value 6) i.ranges_upper;
            read_upper <-- read_upper.value +: vddn 64;
            read_lower <-- read_lower.value +: vddn 64;
            sm.set_next S_read_write;
          ];
          when_ (write_upper.value ==: Signal.of_int ~width:64 range_length)[
            read_done <-- vdd;
            read_lower <-- gndn 64;
            read_upper <-- gndn 64;
            write_lower <-- gndn 64;
            write_upper <-- gndn 64;
            idx <-- gndn 64;
            sm.set_next S_sort;
          ]
        ])
        ;
        (S_read_write, [
          write_lower_enable <-- vdd;
          write_upper_enable <-- vdd;
          write_lower <-- write_lower.value +: vddn 64;
          write_upper <-- write_upper.value +: vddn 64;
          sm.set_next S_read_read;
        ])
        ;
        (S_sort, [
          mn_lower <-- Signal.of_int ~width:64 100000000000000000; (*1e17*)
          mn_upper <-- Signal.of_int ~width:64 100000000000000000; (*1e17*)
          read_lower <-- idx.value;
          read_upper <-- idx.value;
          if_ (idx.value ==: Signal.of_int ~width:64 range_length)
          [
            read_lower <-- gndn 64;
            read_upper <-- gndn 64;
            read_done <-- vdd;
            sm.set_next S_part1;
          ]
          [
            sm.set_next S_compare_read;
          ];
        ])
        ;
        (S_compare_read, [
          lower_swap <-- lower.(0);
          upper_swap <-- upper.(0);
          mn_lower <-- lower.(0);
          mn_upper <-- upper.(0);
          mn_idx <-- idx.value;
          read_lower <-- idx.value +: vddn 64;
          read_upper <-- idx.value +: vddn 64;
          sm.set_next S_compare;
        ])
        ;
        (S_compare, [
          if_ (read_lower.value ==: Signal.of_int ~width:64 (range_length+1))
          [
            write_lower <-- mn_idx.value;
            write_upper <-- mn_idx.value;
            write_lower_data <-- lower_swap.value;
            write_upper_data <-- upper_swap.value;
            sm.set_next S_swap_1;
          ]
          [
            when_ (mn_lower.value >: lower.(0))
            [
              mn_lower <-- lower.(0);
              mn_upper <-- upper.(0);
              mn_idx <-- (read_lower.value -: vddn 64);
            ];
            read_lower <-- read_lower.value +: vddn 64;
            read_upper <-- read_upper.value +: vddn 64;
            sm.set_next S_compare;
          ];
        ])
        ;
        (S_swap_1, [
          write_lower_enable <-- vdd;
          write_upper_enable <-- vdd;
          write_lower <-- idx.value;
          write_upper <-- idx.value;
          write_lower_data <-- mn_lower.value;
          write_upper_data <-- mn_upper.value;
          sm.set_next S_swap_2;
        ])
        ;
        (S_swap_2, [
          write_lower_enable <-- vdd;
          write_upper_enable <-- vdd;
          idx <-- idx.value +: vddn 64;
          read_lower <-- idx.value +: vddn 64;
          read_upper <-- idx.value +: vddn 64;
          sm.set_next S_sort;
        ])
        ;
        (S_part1, [
          if_ (read_lower.value ==: Signal.of_int ~width:64 (range_length+1))
          [
            query_cnt <-- query_cnt.value +: vddn 64;
            read_lower <-- gndn 64;
            read_upper <-- gndn 64;
            read_done <-- vdd;
          ]
          [
            if_ ((i.query >=: lower.(0)) &: (i.query <=: upper.(0)))
            [
              read_lower <-- gndn 64;
              read_upper <-- gndn 64;
              sm.set_next S_add_ans;
            ]
            [
              read_lower <-- read_lower.value +: vddn 64;
              read_upper <-- read_upper.value +: vddn 64;
            ];
          ];
          when_ (query_cnt.value ==: Signal.of_int ~width:64 query_length)
          [
            read_lower <-- gndn 64;
            read_upper <-- gndn 64;
            cur_tail <-- gndn 64;
            sm.set_next S_part2;
          ];
        ])
        ;
        (S_add_ans, [
          sm.set_next S_part1;
          ans_part1 <-- ans_part1.value +: vddn 64;
          query_cnt <-- query_cnt.value +: vddn 64;
          read_done <-- vdd;
        ])
        ;
        (S_part2, [
          if_ (lower.(0) >: cur_tail.value)
          [
            ans_part2 <-- ans_part2.value +: (upper.(0) -: lower.(0)) +: vddn 64;
          ]
          [
            if_ (upper.(0) >: cur_tail.value)
            [
              ans_part2 <-- ans_part2.value +: upper.(0) -: cur_tail.value;
            ]
            [];
          ];
          read_lower <-- read_lower.value +: vddn 64;
          read_upper <-- read_upper.value +: vddn 64;
          when_ (upper.(0) >: cur_tail.value)
          [
            cur_tail <-- upper.(0);
          ];
          when_ (read_lower.value ==: Signal.of_int ~width:64 (range_length + 1))
          [
            sm.set_next S_done;
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
      read_done = read_done.value;
      _done = _done.value;
    }
  end
end