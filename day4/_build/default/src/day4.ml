open Hardcaml
open Base
open Signal

module type Config = sig
  val row_size : int
  val col_size : int
end

module Day4 (Config : Config) = struct
  open Config
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      row_size : 'a [@bits 32];
      col_size : 'a [@bits 32];
      map : 'a list [@bits 1] [@length row_size * col_size];
  } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      ans_part1 : 'a[@bits 32];
      ans_part2 : 'a[@bits 32];
      state : 'a[@bits 4];
      _done : 'a[@bits 1];
    } [@@deriving hardcaml]
  end

  let directions = [1;2;3;4;5;6;7;8]

  module States = struct
    type t =
      | S_wait
      | S_reading
      | S_padding_up
      | S_padding_left
      | S_padding_right
      | S_padding_down
      | S_counting_first
      | S_counting
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (i : _ I.t) =
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    let sm = State_machine.create (module States) r_sync in
    let x = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let y = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let vddn n = Signal.uresize vdd n in
    let gndn n = Signal.uresize gnd n in
    let result = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let read_idx = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let write_idx = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let write_en = Always.Variable.wire ~default:gnd in
    let write_in = Always.Variable.wire ~default:gnd in
    (* let col_size = i.col_size in *)
    let new_col_size = i.col_size +: vddn 32 +: vddn 32 in
    (* let new_row_size = i.row_size +: vddn 32 +: vddn 32 in *)
    let new_total = ((Config.col_size + 2) * (Config.row_size + 2)) in
    let read_port_0 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize read_idx.value (Int.ceil_log2 new_total);
    } in
    let read_port_1 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value -: new_col_size -: vddn 32) (Int.ceil_log2 new_total);
    } in
    let read_port_2 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value -: new_col_size) (Int.ceil_log2 new_total);
    } in
    let read_port_3 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value -: new_col_size +: vddn 32) (Int.ceil_log2 new_total);
    } in
    let read_port_4 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value -: vddn 32) (Int.ceil_log2 new_total);
    } in
    let read_port_5 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value +: vddn 32) (Int.ceil_log2 new_total);
    } in
    let read_port_6 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value +: new_col_size -: vddn 32) (Int.ceil_log2 new_total);
    } in
    let read_port_7 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value +: new_col_size) (Int.ceil_log2 new_total);
    } in
    let read_port_8 =
    {
      Read_port.read_clock = i.clock;
      read_enable = vdd;
      read_address = Signal.uresize (read_idx.value +: new_col_size +: vddn 32) (Int.ceil_log2 new_total);
    } in
    let ram = 
      (Ram.create 
        ~collision_mode:Read_before_write
        ~size:new_total
        ~write_ports:
        [| {
          write_clock = i.clock;
          write_enable = write_en.value;
          write_address = Signal.uresize write_idx.value (Int.ceil_log2 new_total);
          write_data = write_in.value;
        }|]
        ~read_ports:
        [| read_port_0; read_port_1; read_port_2; read_port_3; read_port_4; read_port_5; read_port_6; read_port_7; read_port_8|]
        ())
    in
    let main_read = ram.(0) in
    let neighbor_cnt : Signal.t = List.fold_left 
      ~f:(fun (acc : Signal.t) x -> 
        let cell_n = ram.(x) in
        let one_bit4 = Signal.uresize cell_n 4 in
        acc +: one_bit4
      )
      ~init:(Signal.zero 4)
      directions
    in
    let ans_part1 = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let ans_part2 = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
    let _done = Always.Variable.wire ~default:gnd in
    Always.(
      compile [ sm.switch[
        ( S_wait
        , [ x <-- gndn 32;
            y <-- gndn 32;
            write_idx <-- gndn 32;
            read_idx <-- gndn 32;
            result <-- zero 32;
            _done <-- gnd;
            sm.set_next S_padding_up
          ] )
        ; 
        ( S_padding_up
        , [ write_en <-- vdd;
            write_in <-- gnd;
            write_idx <-- write_idx.value +: vddn 32;
            if_ (write_idx.value ==: (new_col_size -: vddn 32)) [
              sm.set_next S_padding_left;
            ][]
          ])
        ;
        ( S_padding_right
        , [
            write_en <-- vdd;
            write_in <-- gnd;
            write_idx <-- write_idx.value +: vddn 32;
            sm.set_next S_padding_left;
          ])
        ;
        ( S_padding_left
        , [ write_en <-- vdd;
            write_in <-- gnd;
            write_idx <-- write_idx.value +: vddn 32;
            sm.set_next S_reading;
          ])
        ;
        ( S_padding_down
        , [
          write_en <-- vdd;
          write_in <-- gnd;
          write_idx <-- write_idx.value +: vddn 32;
          if_ (write_idx.value ==: Signal.of_int ~width:32 (new_total - 1)) [
            sm.set_next S_counting_first;
            x <-- gndn 32;
            y <-- gndn 32;
            read_idx <-- new_col_size +: vddn 32;
            write_idx <-- new_col_size +: vddn 32;
          ]
          []
        ])
        ;
        ( S_reading
        , [ write_en <-- vdd;
            write_in <-- mux read_idx.value i.map;
            write_idx <-- write_idx.value +: vddn 32;
            if_ (y.value +: vddn 32 <: i.col_size) [
              y <-- y.value +: vddn 32
            ][
              x <-- x.value +: vddn 32;
              y <-- gndn 32;
              sm.set_next S_padding_right;
            ];
            read_idx <-- Signal.uresize (x.value *: i.col_size) 32 +: y.value +: vddn 32;
            if_ ((x.value +: vddn 32 ==: i.row_size) &: (y.value +: vddn 32 ==: i.col_size)) [
              sm.set_next S_padding_down;
            ]
            [];
          ])
        ;
        ( S_counting_first
        ,[if_ (main_read ==: vdd) [
            if_ (neighbor_cnt <: Signal.of_int ~width:4 4) [
              result <-- result.value +: vddn 32;
            ]
            []
          ]
          [];
          if_ (y.value +: vddn 32 <: i.col_size) [
            y <-- y.value +: vddn 32;
            read_idx <-- Signal.uresize ((x.value +: vddn 32) *: new_col_size) 32 +: y.value +: vddn 32 +: vddn 32;
            write_idx <-- Signal.uresize ((x.value +: vddn 32) *: new_col_size) 32 +: y.value +: vddn 32; (*Funny fact that this write_idx always late for one cycle*)
          ]
          [
            x <-- x.value +: vddn 32;
            y <-- gndn 32;
            read_idx <-- Signal.uresize ((x.value +: vddn 32 +: vddn 32) *: new_col_size) 32 +: vddn 32;
            write_idx <-- Signal.uresize ((x.value +: vddn 32) *: new_col_size) 32 +: y.value +: vddn 32; (*Funny fact that this write_idx always late for one cycle*)
          ];
          if_ ((x.value +: vddn 32 ==: i.row_size) &: (y.value +: vddn 32 ==: i.col_size)) [
            sm.set_next S_done
          ]
          [];
          ]);
        ( S_counting
        , [
          if_ (main_read ==: vdd) [
            if_ (neighbor_cnt <: Signal.of_int ~width:4 4) [
              result <-- result.value +: vddn 32;
              write_en <-- vdd;
              write_in <-- gnd;
            ]
            []
          ]
          [];
          if_ (y.value +: vddn 32 <: i.col_size) [
            y <-- y.value +: vddn 32;
            read_idx <-- Signal.uresize ((x.value +: vddn 32) *: new_col_size) 32 +: y.value +: vddn 32 +: vddn 32;
            write_idx <-- Signal.uresize ((x.value +: vddn 32) *: new_col_size) 32 +: y.value +: vddn 32; (*Funny fact that this write_idx always late for one cycle*)
          ]
          [
            x <-- x.value +: vddn 32;
            y <-- gndn 32;
            read_idx <-- Signal.uresize ((x.value +: vddn 32 +: vddn 32) *: new_col_size) 32 +: vddn 32;
            write_idx <-- Signal.uresize ((x.value +: vddn 32) *: new_col_size) 32 +: y.value +: vddn 32; (*Funny fact that this write_idx always late for one cycle*)
          ];
          (* if_ ((x.value +: vddn 32 ==: i.row_size) &: (y.value +: vddn 32 ==: i.col_size)) [ *)
          if_ (x.value ==: i.row_size) [
            sm.set_next S_done
          ]
          [];
        ]);
        ( S_done, 
          [ 
            if_ ((ans_part1.value ==: gndn 32) &: (result.value <>: gndn 32)) [
              ans_part1 <-- result.value;
              result <-- zero 32;
              x <-- gndn 32;
              y <-- gndn 32;
              read_idx <-- new_col_size +: vddn 32;
              write_idx <-- new_col_size +: vddn 32;
              sm.set_next S_counting;
            ]
            [if_ (ans_part2.value ==: result.value)
            [
              _done <-- vdd;
            ]
            [
              ans_part2 <-- result.value;
              sm.set_next S_counting;
              x <-- gndn 32;
              y <-- gndn 32;
              read_idx <-- new_col_size +: vddn 32;
              write_idx <-- new_col_size +: vddn 32;
              write_en <-- vdd;
              write_in <-- gnd;
            ];];
            
          ])
        ] ;
      ]
    );
    { O.ans_part1 = ans_part1.value;
      ans_part2 = ans_part2.value;
      state = sm.current;
      _done = _done.value;}
  ;;
end