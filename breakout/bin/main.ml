(* Basic Breakout Implementation *)
open Breakout_tools

module Objs = Objects

let screen_width = 960
let screen_height = 720

let setup () =
  let open Raylib in

  init_window screen_width screen_height "Breakout";
  set_target_fps (get_monitor_refresh_rate 0);
  set_exit_key Key.Null;

  Objs.State.reset ()

let rec loop (state : Objs.State.t) =
  match (Raylib.window_should_close ()) with
  | true -> Raylib.close_window ()
  | false ->
    let open Raylib in

    (* Cleanup Code *)
    let exit_button_bounds = Rectangle.create 885. 25. 50. 50. in
    if (Objs.State.button_pressed exit_button_bounds) then
      close_window () else ();
    
    let settings_text = "Settings" in
    let settings_width = (measure_text settings_text 48) in
    let settings_y = screen_height / 4 + 48 * 2 in
    let settings_button_bounds = Rectangle.create
      (float_of_int (screen_width / 2 - settings_width / 2))
      (float_of_int settings_y)
      (float_of_int settings_width)
      (float_of_int 48)
    in

    let dim_x = get_screen_width () * 3 / 4 in
    let dim_y = get_screen_height () * 3 / 4 in
    let pos_x = (get_screen_width () - dim_x) / 2 in
    let pos_y = (get_screen_height () - dim_y) / 2 in

    let box = Rectangle.create (float_of_int pos_x) (float_of_int pos_y) (float_of_int dim_x) (float_of_int dim_y) in

    let state =
      if (is_key_pressed Key.R) then Objs.State.reset ()
      else if (is_key_pressed Key.Escape || state.lives = 0 || List.is_empty state.blocks) then {state with pause = true}
      else if is_key_pressed Key.Up then {state with pause = false}
      else if (state.pause && Objs.State.button_pressed settings_button_bounds) then {state with menu = true}
      else if (is_mouse_button_pressed MouseButton.Left && not (Objs.State.button_pressed box)) then {state with menu = false}
      else state
    in

    let rate = (1. /. float_of_int (get_fps ())) in

    let state =
      if state.pause then
        state
      else
        let {Objs.Paddle.position; velocity; dimensions} = state.paddle in

        let velocity =
          if (is_key_down Key.Left && (Vector2.x position) > 0.) then -900. *. rate
          else if (is_key_down Key.Right &&
            (Vector2.x position) < ((float_of_int screen_width) -. (Vector2.x dimensions))) then 900. *. rate
          else 0.
        in

        let position = Vector2.create
          ((Vector2.x position) +. velocity)
          (Vector2.y position)
        in

        let paddle = {Objs.Paddle.position; velocity; dimensions} in
        
        {state with paddle}
    in

    let state =
      if state.pause then
        state
      else
        let {Objs.Ball.position; velocity; radius} = state.ball in

        let dmg = Objs.State.ball_reset_required state.ball state.paddle in

        let velocity = 
          if (dmg) then
            let vx, vy = Objs.State.random_ball_velocity () in
            Vector2.create vx vy
          else Objs.State.collision_handling state.ball state.paddle state.blocks
        in

        let lives = if (dmg) then state.lives - 1 else state.lives in

        let blocks = List.filter (fun (block : Objs.Block.t) ->
          not (
            (Objs.State.block_collision state.ball block)
          )
        ) state.blocks in

        let position =
          if (dmg) then
            Vector2.create (float_of_int screen_width /. 2.) (float_of_int screen_height /. 2.)
          else
            Vector2.create (Vector2.x position +. Vector2.x velocity *. rate) (Vector2.y position +. Vector2.y velocity *. rate)
        in

        let ball = {Objs.Ball.position; velocity; radius} in
        {state with ball; blocks; score = state.score + (List.length state.blocks - List.length blocks); lives}
    in
    
    if (state.lives = 0) then Objs.State.game_over ()
    else if (List.is_empty state.blocks) then Objs.State.game_winner state.score
    else Objs.State.draw state;
    loop state
  
let () = setup () |> loop