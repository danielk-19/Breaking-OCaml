(* Basic Breakout Implementation *)
open Breakout_tools

module Objs = Objects

let screen_width = 960
let screen_height = 720

let setup () =
  let open Raylib in

  init_window screen_width screen_height "Breakout";
  set_target_fps 60;

  let paddle = 
    let dim_x = 300. in
    let dim_y = 50. in
    let pos_x = ((float_of_int screen_width) /. 2. -. dim_x /. 2.) in
    let pos_y = ((float_of_int screen_height) -. dim_y /. 2. -. 50.) in  

    let position = Vector2.create pos_x pos_y in
    let velocity = 0. in
    let dimensions = Vector2.create dim_x dim_y in
    {Objs.Paddle.position; velocity; dimensions}
  in

  let ball =
    let rad = 10. in
    let pos_x = ((float_of_int screen_width) /. 2. -. rad) in
    let pos_y = ((float_of_int screen_height) /. 2. -. rad) in

    let position = Vector2.create pos_x pos_y in
    let velocity = Vector2.create 0. 0. in
    let radius = (int_of_float rad) in
    {Objs.Ball.position; velocity; radius}
  in

  {Objs.State.paddle; ball; pause = true; frames_counter = 0};;

let rec loop (state : Objs.State.t) =
  match (Raylib.window_should_close ()) with
  | true -> Raylib.close_window ()
  | false ->
    let open Raylib in

    let state =
      if is_key_pressed Key.Up then {state with pause = false}
      else if is_key_pressed Key.Space then {state with pause = true}
      else state
    in

    let state =
      if state.pause then
        state
      else
        let {Objs.Paddle.position; velocity; dimensions} = state.paddle in
        let velocity =
          if (is_key_pressed_repeat Key.Left && (Vector2.x position) > 0.) then -15.
          else if (is_key_pressed_repeat Key.Right && (Vector2.x position) < ((float_of_int screen_width) -. (Vector2.x dimensions))) then 15.
          else 0.
        in

        let position = Vector2.create
          ((Vector2.x position) +. velocity)
          (Vector2.y position)
        in

        let paddle = {Objs.Paddle.position; velocity; dimensions} in
        {state with paddle}
    in

    Objs.State.draw state;
    loop state
  
let () = setup () |> loop;;