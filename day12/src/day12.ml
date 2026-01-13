open Hardcaml
open Base
open Signal

module Day12 = struct
  module I = struct
    type 'a t = {
      clock : 'a [@bits 1];
      clear : 'a [@bits 1];
      length : 'a [@bits 64];
      row : 'a [@bits 64];
      col : 'a [@bits 64];
      pattern_cnt : 'a list [@length 6] [@bits 64];
      presents : 'a list [@length 6] [@bits 64];
    }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      ans : 'a [@bits 64];
      _done : 'a [@bits 1];
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = 
      | S_wait
      | S_read
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end
  let create (i : _ I.t) = begin
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) r_sync in
    let idx = Always.Variable.reg ~width:64 r_sync in
    let cnt = Always.Variable.reg ~width:64 r_sync in
    let ans = Always.Variable.reg ~width:64 r_sync in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let thirteen = of_int ~width:64 13 in
    let ten = of_int ~width:64 10 in
    let _done = Always.Variable.wire ~default:gnd in
    let read_done = Always.Variable.wire ~default:gnd in
    Always.(compile[
      sm.switch[
        (S_wait, [
          idx <-- gndn 64;
          cnt <-- gndn 64;
          read_done <-- vdd;
          sm.set_next S_read;
        ])
        ;
        (S_read, [
          if_ (idx.value ==: of_int ~width:64 6)
          [
            when_ (uresize (cnt.value *: thirteen) 64 <: uresize (i.col *: i.row *: ten) 64)
            [
              ans <-- ans.value +: vddn 64;
            ];
            sm.set_next S_done;
          ]
          [
            idx <-- idx.value +: vddn 64;
            cnt <-- cnt.value +: uresize ((mux (uresize idx.value 3) i.presents) *: (mux (uresize idx.value 3) i.pattern_cnt)) 64;
          ]
          ;
        ])
        ;
        (S_done, [
          _done <-- vdd;
          sm.set_next S_wait;
        ])
      ]
    ]);
    {
      O.ans = ans.value;
      _done = _done.value;
    }
  end
end