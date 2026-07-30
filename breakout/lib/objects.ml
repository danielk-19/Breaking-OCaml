(* Implemented Modules for Breakout *)

module Ball =
    struct
        type t = {
            mutable position : Raylib.Vector2.t;
            mutable velocity : Raylib.Vector2.t;
            radius : int;
        }
    end

module Paddle =
    struct
        type t = {
            mutable position : Raylib.Vector2.t;
            mutable velocity : float;
            dimensions : Raylib.Vector2.t;
        }
    end

module Block =
    struct
        type t = {
            position : Raylib.Vector2.t;
            dimensions : Raylib.Vector2.t;
            color : Raylib.Color.t;
        }
    end

module State =
    struct
        type t = {
            paddle : Paddle.t;
            ball : Ball.t;
            blocks : Block.t list;
            mutable pause : bool;
            mutable score : int;
            mutable lives : int;
            mutable frame_counter : int
        }
        
        (* let collision (ball : Ball.t) (block : Block.t) =
            let open Raylib in
            (((Vector2.y ball.position +. float_of_int ball.radius) >= Vector2.y block.position &&
            (Vector2.y ball.position -. float_of_int ball.radius) <= (Vector2.y block.position +. Vector2.y block.dimensions)) &&
            ((Vector2.x ball.position +. float_of_int ball.radius) >= Vector2.x block.position &&
            (Vector2.x ball.position -. float_of_int ball.radius) <= (Vector2.x block.position +. Vector2.x block.dimensions))) *)

        let wall_collision_x (ball : Ball.t) =
            let open Raylib in
            (Vector2.x ball.position <= float_of_int ball.radius ||
            Vector2.x ball.position >= float_of_int (get_screen_width () - ball.radius))

        let wall_collision_y (ball : Ball.t) =
            let open Raylib in
            (Vector2.y ball.position <= float_of_int ball.radius)

        let ball_reset_required (ball : Ball.t) (paddle : Paddle.t) =
            let open Raylib in
            ((Vector2.y ball.position -. float_of_int ball.radius) >= Vector2.y paddle.position +. 50.)

        let block_collision (ball : Ball.t) (block : Block.t) =
            let open Raylib in
            check_collision_circle_rec ball.position (float_of_int ball.radius) (Rectangle.create (Vector2.x block.position) (Vector2.y block.position) (Vector2.x block.dimensions) (Vector2.y block.dimensions))
        
        let paddle_collision (ball : Ball.t) (paddle : Paddle.t) =
            let open Raylib in
            check_collision_circle_rec ball.position (float_of_int ball.radius) (Rectangle.create (Vector2.x paddle.position) (Vector2.y paddle.position) (Vector2.x paddle.dimensions) (Vector2.y paddle.dimensions))
        
        let paddle_hit (ball : Ball.t) (paddle : Paddle.t) =
            let open Raylib in
            let hit = (((Vector2.x ball.position -. Vector2.x paddle.position) /. Vector2.x paddle.dimensions) -. 0.5) *. 2.0 in
            Float.max (-1.) (Float.min hit 1.) *. 1.0472

        let ball_wall_collision_velocity (ball : Ball.t) =
            let open Raylib in
            let vx =
                if (wall_collision_x ball) then
                    -.(Vector2.x ball.velocity)
                else (Vector2.x ball.velocity)
            in

          (*||
            (((Vector2.y position +. float_of_int radius) -. Vector2.y state.paddle.position) >= 0. &&
            ((Vector2.x position +. float_of_int radius) >= Vector2.x state.paddle.position &&
            (Vector2.x position -. float_of_int radius) <= (Vector2.x state.paddle.position +. Vector2.x state.paddle.dimensions)))*)
            let vy =
                if (wall_collision_y ball) then
                    -.(Vector2.y ball.velocity)
                else (Vector2.y ball.velocity)
            in

            Vector2.create vx vy

        let paddle_overlap (ball : Ball.t) (paddle : Paddle.t) =
            let open Raylib in
            let b_x = Vector2.x ball.position in 
            let b_y = Vector2.y ball.position in 
            let p_x = Vector2.x paddle.position in 
            let p_y = Vector2.y paddle.position in 
            let p_w = Vector2.x paddle.dimensions in 
            let p_h = Vector2.y paddle.dimensions in 
            let radius = float_of_int ball.radius in

            (* Determine overlaps *)
            let right_overlap = (p_x +. p_w) -. (b_x -. radius) in
            let left_overlap = (b_x +. radius) -. p_x in
            let top_overlap = (b_y +. radius) -. p_y in
            let bottom_overlap = (p_y +. p_h) -. (b_y -. radius) in

            (* Find the shortest path out *)
            let min_x = Float.min left_overlap right_overlap in
            let min_y = Float.min top_overlap bottom_overlap in

            let x_diff, y_diff =
                if (min_x < min_y) then
                    if (Float.compare left_overlap right_overlap < 0) then (-.(left_overlap), 0.0)
                    else (right_overlap, 0.0)
                else
                    if (Float.compare top_overlap bottom_overlap < 0) then (0.0, -.(top_overlap))
                    else (0.0, bottom_overlap)
            in
            Vector2.create (b_x +. x_diff) (b_y +. y_diff)

        let block_overlap (ball : Ball.t) (block : Block.t) =
            let open Raylib in
            let b_x = Vector2.x ball.position in 
            let b_y = Vector2.y ball.position in 
            let p_x = Vector2.x block.position in 
            let p_y = Vector2.y block.position in 
            let p_w = Vector2.x block.dimensions in 
            let p_h = Vector2.y block.dimensions in 
            let radius = float_of_int ball.radius in

            (* Determine overlaps *)
            let right_overlap = (p_x +. p_w) -. (b_x -. radius) in
            let left_overlap = (b_x +. radius) -. p_x in
            let top_overlap = (b_y +. radius) -. p_y in
            let bottom_overlap = (p_y +. p_h) -. (b_y -. radius) in

            (* Find the shortest path out *)
            let min_x = Float.min left_overlap right_overlap in
            let min_y = Float.min top_overlap bottom_overlap in

            let x_diff, y_diff =
                if (min_x < min_y) then
                    if (Float.compare left_overlap right_overlap < 0) then (-.(left_overlap), 0.0)
                    else (right_overlap, 0.0)
                else
                    if (Float.compare top_overlap bottom_overlap < 0) then (0.0, -.(top_overlap))
                    else (0.0, bottom_overlap)
            in
            Vector2.create (b_x +. x_diff) (b_y +. y_diff)

        let ball_paddle_collision_velocity (ball : Ball.t) (paddle : Paddle.t) =
            let open Raylib in
            let speed = sqrt ((Vector2.x ball.velocity ** 2.) +. (Vector2.y ball.velocity ** 2.)) in

            ball.position <- paddle_overlap ball paddle;

            let angle = (paddle_hit ball paddle) in

            let vx = speed *. sin angle in
            let vy = speed *. cos angle in

            let direction_x = if (Float.sign_bit (Vector2.x ball.velocity) <> Float.sign_bit vx) then -1. else 1. in
            let direction_y = if (Vector2.y ball.position < (Vector2.y paddle.position +. Vector2.y paddle.dimensions /. 2.) && vy = abs_float vy) then -1. else 1. in

            Vector2.create (vx *. direction_x) (vy *. direction_y)
            
        let ball_block_collision_velocity (ball : Ball.t) (block : Block.t) =
            let open Raylib in

            let vx = Vector2.x ball.velocity in
            let vy = Vector2.y ball.velocity in

            let left_overlap = abs_float (Vector2.x block.position -. (Vector2.x ball.position +. (float_of_int ball.radius))) in
            let right_overlap = abs_float ((Vector2.x block.position +. Vector2.x block.dimensions) -. (Vector2.x ball.position -. (float_of_int ball.radius))) in
            let top_overlap = abs_float (Vector2.y block.position -. (Vector2.y ball.position +. (float_of_int ball.radius))) in
            let bottom_overlap = abs_float ((Vector2.y block.position +. Vector2.y block.dimensions) -. (Vector2.y ball.position -. (float_of_int ball.radius))) in

            let min_overlap = Float.min (Float.min left_overlap right_overlap) (Float.min top_overlap bottom_overlap) in

            (* let dir_x =
                if (((min_overlap = bottom_overlap || min_overlap = top_overlap) && Float.sign_bit (Vector2.x ball.velocity) <> Float.sign_bit vx) ||
                    ((min_overlap = left_overlap || min_overlap = right_overlap) && not (Float.sign_bit (Vector2.x ball.velocity) <> Float.sign_bit vx))) then -1.
                else 1.
            in

            let dir_y =
                if (((min_overlap = left_overlap || min_overlap = right_overlap) && Float.sign_bit (Vector2.x ball.velocity) <> Float.sign_bit vx) ||
                    ((min_overlap = bottom_overlap || min_overlap = top_overlap) && not (Float.sign_bit (Vector2.x ball.velocity) <> Float.sign_bit vx))) then -1.
                else 1.
            in *)

            let dir_x, dir_y =
                if (min_overlap = left_overlap || min_overlap = right_overlap) then (-1., 1.)
                else (1., -1.)
            in

            ball.position <- block_overlap ball block;

            Vector2.create (vx *. dir_x) (vy *. dir_y)

        let rec check_block_collision (ball : Ball.t) (blocks : Block.t list) =
          match blocks with
          | [] -> ball.velocity
          | block :: rest ->
            if (block_collision ball block) then ball_block_collision_velocity ball block
            else check_block_collision ball rest

            (* let vx =
              if (Objs.State.block_collision state.ball block) then
                -.(Vector2.x curr_velocity)
              else (Vector2.x curr_velocity)
            in

            let vy =
              if (Objs.State.block_collision state.ball block) then
                -.(Vector2.y curr_velocity)
              else (Vector2.y curr_velocity)
            in *)

        let collision_handling (ball : Ball.t) (paddle : Paddle.t) (blocks : Block.t list) =
            if (paddle_collision ball paddle) then (ball_paddle_collision_velocity ball paddle)
            else if (wall_collision_x ball || wall_collision_y ball) then (ball_wall_collision_velocity ball)
            else check_block_collision ball blocks

            (*&&
            ((Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) >= (Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) &&
            (Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) >= (Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius)) ||
            ((Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius) >= (Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) &&
            (Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius) >= (Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius))
*)
        (* let collision_y (ball : Ball.t) (block : Block.t) =
            let open Raylib in
            check_collision_circle_rec ball.position (float_of_int ball.radius) (Rectangle.create (Vector2.x block.position) (Vector2.y block.position) (Vector2.x block.dimensions) (Vector2.y block.dimensions))
             *)
            (*&&
            ((Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) >= (Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) &&
            (Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) >= (Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius)) ||
            ((Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius) >= (Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) &&
            (Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius) >= (Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius))
*)      

        let random_ball_velocity () =
            let random_float = Random.float 1. in
            let vel_x = 850. *. random_float in
            let vel_y = sqrt (850. ** 2. -. vel_x ** 2.) in

            let random_num = Random.int 2 in
            let dir_x, dir_y =
                if (random_num = 0) then (1., 1.) else (-1., 1.)
            in

            (vel_x *. dir_x, vel_y *. dir_y)

        let reset () =
            let open Raylib in

            let screen_width = get_screen_width () in
            let screen_height = get_screen_height () in

            let paddle = 
                let dim_x = 300. in
                let dim_y = 50. in
                let pos_x = ((float_of_int screen_width) /. 2. -. dim_x /. 2.) in
                let pos_y = ((float_of_int screen_height) -. dim_y /. 2. -. 50.) in  

                let position = Vector2.create pos_x pos_y in
                let velocity = 0. in
                let dimensions = Vector2.create dim_x dim_y in
                {Paddle.position; velocity; dimensions}
            in

            let ball =
                let rad = 10. in
                let pos_x = ((float_of_int screen_width) /. 2.) in
                let pos_y = ((float_of_int screen_height) /. 2.) in

                let vel_x, vel_y = random_ball_velocity () in

                let position = Vector2.create pos_x pos_y in
                let velocity = Vector2.create vel_x vel_y in
                let radius = (int_of_float rad) in
                {Ball.position; velocity; radius}
            in

            let blocks = 
                let dim_x = 100. in
                let dim_y = 50. in
                let num_blocks_x = 8 in
                let num_blocks_y = 4 in
                let offset = 5. in
                let rec make_blocks x y acc =
                if y >= (dim_y *. float_of_int num_blocks_y +. offset *. float_of_int num_blocks_y +. 50.) then acc
                else if x >= (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x +. (float_of_int screen_width -. (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x)) /. 2.) then
                    make_blocks ((float_of_int screen_width -. (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x)) /. 2.) (y +. dim_y +. offset) acc
                else
                    let block = {Block.position = Vector2.create x y; dimensions = Vector2.create dim_x dim_y; color = Raylib.Color.red} in
                    make_blocks (x +. dim_x +. offset) y (block :: acc)
                in
                make_blocks ((float_of_int screen_width -. (dim_x *. float_of_int num_blocks_x +. offset *. float_of_int num_blocks_x)) /. 2.) 100. []
            in

            {paddle; ball; blocks; pause = true; score = 0; lives = 5; frame_counter = 0}

        let game_winner () =
            let open Raylib in
            begin_drawing ();

            clear_background Color.black;

            let text = "Winner!" in
            let font_size = 48 in
            let font_width = (measure_text text font_size) in            
            draw_text text
                (get_screen_width () / 2 - font_width / 2) (get_screen_height () / 2 - 48)
                font_size Color.white;

            end_drawing ()

        let game_over () =
            let open Raylib in
            begin_drawing ();

            clear_background Color.black;

            let text = "Game Over!" in
            let font_size = 48 in
            let font_width = (measure_text text font_size) in            
            draw_text text
                (get_screen_width () / 2 - font_width / 2) (get_screen_height () / 2 - 48)
                font_size Color.white;

            end_drawing ()

        let draw {paddle; ball; blocks; pause; score; lives; frame_counter} =
            let open Raylib in
            begin_drawing ();

            clear_background Color.darkgray;
            draw_rectangle_v paddle.position paddle.dimensions Color.white;
            draw_circle_v ball.position (float_of_int ball.radius) Color.white;
            
            List.iter (fun (block : Block.t) ->
                draw_rectangle_v block.position block.dimensions block.color
            ) blocks;

            let text =
                if pause then "Press Up Arrow To Continue"
                else ""
            in
            let font_size = 48 in
            let font_width = (measure_text text font_size) in            
            draw_text text
                (get_screen_width () / 2 - font_width / 2) (get_screen_height () / 4)
                font_size Color.white;

            let text = "Score: " ^ string_of_int score in
            let font_size = 24 in
            let font_width = (measure_text text font_size) in
            draw_text text
                (get_screen_width () / 2 - font_width / 2) 25
                font_size Color.white;
            
            let text = "Lives: " ^ string_of_int lives in
            let font_size = 24 in
            draw_text text 25 25 font_size Color.white;

            let dims = 50 in
            let start_pos_x = get_screen_width () - 25 in
            let start_pos_y = 25 in

            let start_pos = Vector2.create (float_of_int (start_pos_x - dims)) (float_of_int start_pos_y) in
            let end_pos = Vector2.create (float_of_int start_pos_x) (float_of_int (start_pos_y + dims)) in

            draw_line_ex start_pos end_pos 10. Color.white;
            
            let start_pos = Vector2.create (float_of_int (start_pos_x - dims)) (float_of_int (start_pos_y + dims)) in
            let end_pos = Vector2.create (float_of_int start_pos_x) (float_of_int start_pos_y) in
            
            draw_line_ex start_pos end_pos 10. Color.white;
            
            end_drawing ()
    end