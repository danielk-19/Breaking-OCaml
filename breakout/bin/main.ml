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
    let pos_x = ((float_of_int screen_width) /. 2.) in
    let pos_y = ((float_of_int screen_height) /. 2.) in

    let position = Vector2.create pos_x pos_y in
    let velocity = Vector2.create 5. 5. in
    let radius = (int_of_float rad) in
    {Objs.Ball.position; velocity; radius}
  in

  let blocks = 
    let dim_x = 200. in
    let dim_y = 100. in
    let num_blocks_x = 4 in
    let num_blocks_y = 2 in
    let offset = 5. in
    let rec make_blocks x y acc =
      if y >= (dim_y *. float_of_int num_blocks_y +. offset *. float_of_int num_blocks_y +. 50.) then acc
      else if x >= (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x +. (float_of_int screen_width -. (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x)) /. 2.) then
        make_blocks ((float_of_int screen_width -. (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x)) /. 2.) (y +. dim_y +. offset) acc
      else
        let block = {Objs.Block.position = Vector2.create x y; dimensions = Vector2.create dim_x dim_y; color = Raylib.Color.red} in
        make_blocks (x +. dim_x +. offset) y (block :: acc)
    in
    make_blocks ((float_of_int screen_width -. (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x)) /. 2.) 100. []
  in

  {Objs.State.paddle; ball; blocks; pause = true; score = 0; frame_counter = 0}

let rec loop (state : Objs.State.t) =
  match (Raylib.window_should_close ()) with
  | true -> Raylib.close_window ()
  | false ->
    let open Raylib in

    let button_bounds = Rectangle.create 885. 25. 50. 50. in
    if (check_collision_point_rec (get_mouse_position ()) button_bounds &&
      is_mouse_button_pressed MouseButton.Left) then
      close_window () else ();
    
    let state =
      if is_key_pressed Key.Up then {state with pause = false}
      else if is_key_pressed Key.Space then {state with pause = true}
      else {state with frame_counter = state.frame_counter + 1}
    in

    let rate = 
      if (get_fps () / 60) = 0 then 1 else (get_fps () / 60)
    in

    let state =
      if state.pause || not (state.frame_counter mod rate = 0) then
        state
      else
        let {Objs.Paddle.position; velocity; dimensions} = state.paddle in

        let velocity =
          if (is_key_down Key.Left && (Vector2.x position) > 0.) then -15.
          else if (is_key_down Key.Right &&
            (Vector2.x position) < ((float_of_int screen_width) -. (Vector2.x dimensions))) then 15.
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
      if state.pause || not (state.frame_counter mod rate = 0) then
        state
      else
        let {Objs.Ball.position; velocity; radius} = state.ball in

        let velocity = 
          let vx =
            if (Vector2.x position <= float_of_int radius ||
            Vector2.x position >= float_of_int (screen_width - radius) (*||
            ((((Vector2.x position +. float_of_int radius) -. Vector2.x state.paddle.position) = 0. &&
            ((Vector2.x position -. float_of_int radius) -. (Vector2.x state.paddle.position +. Vector2.x state.paddle.dimensions)) = 0.) &&
            ((Vector2.y position +. float_of_int radius) >= Vector2.y state.paddle.position &&
            (Vector2.y position -. float_of_int radius) <= (Vector2.y state.paddle.position +. Vector2.y state.paddle.dimensions)))*)) then
              -.(Vector2.x velocity)
            else (Vector2.x velocity)
          in

          let vy =
            if (Vector2.y position <= float_of_int radius ||
            (((Vector2.y position +. float_of_int radius) -. Vector2.y state.paddle.position) >= 0. &&
            ((Vector2.x position +. float_of_int radius) >= Vector2.x state.paddle.position &&
            (Vector2.x position -. float_of_int radius) <= (Vector2.x state.paddle.position +. Vector2.x state.paddle.dimensions)))) then
              -.(Vector2.y velocity)
            else (Vector2.y velocity)
          in

          Vector2.create vx vy
        in

        let position =
          if ((Vector2.y position +. float_of_int radius) >= float_of_int screen_height) then
            Vector2.create (float_of_int screen_width /. 2.) (float_of_int screen_height /. 2.)
          else
            Vector2.create (Vector2.x position +. Vector2.x velocity) (Vector2.y position +. Vector2.y velocity)
        in

        let rec check_block_collision blocks curr_velocity =
          match blocks with
          | [] -> curr_velocity
          | block :: rest ->
            let vx =
              if (Objs.State.collision_x state.ball block) then
                -.(Vector2.x curr_velocity)
              else (Vector2.x curr_velocity)
            in

            let vy =
              if (Objs.State.collision_y state.ball block) then
                -.(Vector2.y curr_velocity)
              else (Vector2.y curr_velocity)
            in

            let new_velocity = Vector2.create vx vy in
            check_block_collision rest new_velocity
        in

        let velocity = check_block_collision state.blocks velocity in

        let blocks = List.filter (fun (block : Objs.Block.t) ->
          not (
            (Objs.State.collision_x state.ball block) &&
            (Objs.State.collision_y state.ball block)
          )
        ) state.blocks in

        let ball = {Objs.Ball.position; velocity; radius} in
        {state with ball; blocks; score = state.score + (List.length state.blocks - List.length blocks)}
    in
    Objs.State.draw state;
    loop state
  
let () = setup () |> loop