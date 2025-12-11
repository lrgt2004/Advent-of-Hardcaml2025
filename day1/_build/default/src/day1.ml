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
      directions : 'a list [@length length] [@bits 1]; (*gnd for left, vdd for right*)
      rotations : 'a list [@length length] [@bits 32];
    }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      ans_part1 : 'a [@bits 32];
      ans_part2 : 'a [@bits 32];
      angle : 'a [@bits 32];
      state : 'a [@bits 2];
      _done : 'a [@bits 1];
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | S_wait
      | S_read
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
    let idx = Always.Variable.reg ~width:32 r_sync in
    let remaining = Always.Variable.reg ~width:32 r_sync in
    let ans_part1 = Always.Variable.reg ~width:32 r_sync in
    let ans_part2 = Always.Variable.reg ~width:32 r_sync in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let rec get_div (a : Signal.t) (b : Signal.t) (acc : Signal.t) = 
      if to_bool (Signal.uresize (a >: b) 1) then
        get_div (a -: b) b acc +: vddn 32
      else acc
    in
    let get_mod (a : Signal.t) (b : Signal.t) =  
      a -: (b *: get_div a b (gndn 32))
    in
    let _done = Always.Variable.reg ~width:1 r_sync in
    Always.(
      compile[ sm.switch[
      (S_wait,
      [
        idx <-- gnd;
        angle <-- Signal.of_int ~width:32 50;
        remaining <-- gnd;
        ans_part1 <-- gnd;
        ans_part2 <-- gnd;
        _done <-- gnd;
        direction <-- gnd;
        sm.set_next S_read;
      ])
      ;
      (S_read,
      [
        if_ (idx.value ==: Signal.of_int ~width:32 length) 
        [
          sm.set_next S_done;
        ]
        [remaining <-- get_mod (mux idx.value i.rotations) (Signal.of_int ~width:32 100);
        direction <-- mux idx.value i.directions;
        ans_part2 <-- ans_part2.value +: get_div (mux idx.value i.rotations) (Signal.of_int ~width:32 100) (gndn 32);
        sm.set_next S_rotate;
        idx <-- idx.value +: vddn 32;
        ]
      ])
      ;
      (S_rotate,
      [
        remaining <-- remaining.value -: vddn 32;
        if_ (remaining.value ==: gnd)
        [
          if_ (angle.value ==: gnd)
          [
            ans_part1 <-- ans_part1.value +: vddn 32;
            ans_part2 <-- ans_part2.value +: vddn 32;
          ]
          [];
          sm.set_next S_read;
        ]
        [
          if_ (direction.value ==: gnd)
          [ (*left*)
            if_ (angle.value <>: gndn 32) 
            [
              angle <-- angle.value -: vddn 32;
            ]
            [
              angle <-- Signal.of_int ~width:32 99;
              ans_part2 <-- ans_part2.value +: vddn 32;
            ];
          ]
          [ (*right*)
            angle <-- get_mod (angle.value +: vddn 32) (Signal.of_int ~width:32 100);
            if_ (angle.value ==: gndn 32)
            [
              ans_part2 <-- ans_part2.value +: vddn 32;
            ]
            [];
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
      angle = angle.value;
      state = sm.current;
      _done = _done.value;
    }
  end
end