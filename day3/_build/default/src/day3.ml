open Base
open Hardcaml
open Signal

module type Config = sig
  val length : int
  val num : int
end

module Day3 (Config : Config) = struct
  module I = struct
    type 'a t = {
      clock : 'a [@bits 1];
      clear : 'a [@bits 1];
      num : 'a [@bits 64];
      length : 'a [@bits 64];
      seq : 'a list [@length 100] [@bits 4];
      _width : 'a [@bits 64];
    }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      ans : 'a [@bits 64];
      state : 'a [@bits 2];
      get_next : 'a [@bits 1];
      _done : 'a [@bits 1];
    }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = 
      | S_wait
      | S_find_kth_num
      | S_prepare_for_next_digit
      | S_done
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (i : _ I.t) = begin
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) r_sync in
    let ans = Always.Variable.reg ~width:64 r_sync in
    let cnt = Always.Variable.reg ~width:64 r_sync in
    let result = Always.Variable.reg ~width:64 r_sync in
    let digit = Always.Variable.reg ~width:64 r_sync in
    let prev = Always.Variable.reg ~width:64 r_sync in
    let idx = Always.Variable.reg ~width:64 r_sync in
    let mx = Always.Variable.reg ~width:64 r_sync in
    let gndn x = Signal.uresize gnd x in
    let vddn x = Signal.uresize vdd x in
    let _done = Always.Variable.reg ~width:1 r_sync in
    let one_line_done = Always.Variable.wire ~default:gnd in
    Always.(compile[
      sm.switch[
        (S_wait, [
          ans <-- gndn 64;
          result <-- gndn 64;
          digit <-- i._width;
          prev <-- Signal.of_int ~width:64 (-1);
          idx <-- gndn 64;
          mx <-- gndn 64;
          cnt <-- gndn 64;
          sm.set_next S_find_kth_num;
        ])
        ;
        (S_find_kth_num, [
          if_ (idx.value ==: i.length -: digit.value +: vddn 64)
          [
            sm.set_next S_prepare_for_next_digit;
          ]
          [if_ (Signal.uresize (mux (Signal.uresize idx.value 7) i.seq) 64 >: mx.value) 
          [
            mx <-- Signal.uresize (mux (Signal.uresize idx.value 7) i.seq) 64;
            prev <-- idx.value;
          ]
          [];];
          idx <-- idx.value +: vddn 64;
        ])
        ;
        (S_prepare_for_next_digit, [
          result <-- Signal.uresize (result.value *: Signal.of_int ~width:64 10) 64 +: mx.value;
          digit <-- digit.value -: vddn 64;
          idx <-- prev.value +: vddn 64;
          mx <-- gndn 64;
          if_ (digit.value ==: vddn 64) 
            [sm.set_next S_done]
            [sm.set_next S_find_kth_num;]
        ])
        ;
        (S_done, [
          ans <-- ans.value +: result.value;
          result <-- gndn 64;
          prev <-- Signal.of_int ~width:64 (-1);
          idx <-- gndn 64;
          one_line_done <-- vdd;
          digit <-- i._width;
          cnt <-- cnt.value +: vddn 64;
          if_ (cnt.value ==: i.num -: vddn 64)
          [_done <-- vdd;]
          [sm.set_next S_find_kth_num;]
        ])
      ]
    ]);
    {
      O.ans = ans.value;
      state = sm.current;
      _done = _done.value;
      get_next = one_line_done.value;
    }
  end
end