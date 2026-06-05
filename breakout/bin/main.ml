open Bogue
module W = Widget
module L = Layout
(*
let () =
  W.label "Hello world"
    |> L.resident
    |> Bogue.of_layout
    |> Bogue.run
*)

let screen () =
  let title = W.label "Breakout" in
  let start_button = W.button ~border_radius:10 "START" in

  let paddle_style = Style.(
    create
      ~background:(color_bg Draw.(transp RGB.black))
    ()
  ) in

  let paddle = W.box ~w:100 ~h:20 ~style:paddle_style () in

  let border =
    L.flat_of_w ~align:Draw.Center
    ~background:(L.color_bg Draw.(transp RGB.darkblue))
    [title]
  in
  (*let t1 = L.resident ~background:(L.color_bg Draw.(transp RGB.white)) title in*)
  let b1 = L.resident start_button in
  let p1 = L.resident paddle in
  let layout = L.tower ~sep:10 ~align:Draw.Center [border; b1; p1] in
  
  let board = Main.of_layout layout in
  Main.run board

let () = screen ()