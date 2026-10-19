open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    }
      [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { io_voltage : 'a
    }
      [@@deriving hardcaml]
end

(* 0.5s delay = toggle every 25M clock cycles for 50MHz *)
let count_value = 24_999_999

let create (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock () in
  (*25bit counter*)
  let count =
    reg_fb spec ~width:25 ~f:(fun count ->
             mux2
               (count ==:. count_value)
               (zero 25)
               (count +:. 1))
  in
  (*toggle output at terminal count*)
  let led =
    reg_fb spec ~width:1 ~f:(fun led ->
             mux2
               (count ==:. count_value)
               (~:led)
               led)
  in
  { O.io_voltage = led }
let () =
  let clock = Signal.input "Clock" 1 in
  let outputs = create { I.clock } in

  let circuit =
    Circuit.create_exn
      ~name:"led"
      [ Signal.output "io_voltage" outputs.io_voltage ]
  in
  Rtl.output
    ~output_mode:(Rtl.Output_mode.To_file "led.v")
    Verilog
    circuit
