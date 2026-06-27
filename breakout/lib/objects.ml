(* Implemented Modules for Breakout *)

module Ball =
    struct
        type t = {
            position : Raylib.Vector2.t;
            velocity : Raylib.Vector2.t;
            radius : int;
        }
    end

module Paddle =
    struct
        type t = {
            position : Raylib.Vector2.t;
            velocity : float;
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
            pause : bool;
            score: int;
            frame_counter: int
        }
        
        (* let collision (ball : Ball.t) (block : Block.t) =
            let open Raylib in
            (((Vector2.y ball.position +. float_of_int ball.radius) >= Vector2.y block.position &&
            (Vector2.y ball.position -. float_of_int ball.radius) <= (Vector2.y block.position +. Vector2.y block.dimensions)) &&
            ((Vector2.x ball.position +. float_of_int ball.radius) >= Vector2.x block.position &&
            (Vector2.x ball.position -. float_of_int ball.radius) <= (Vector2.x block.position +. Vector2.x block.dimensions))) *)

        (* Fixing Error Currently *)
        let collision_x (ball : Ball.t) (block : Block.t) =
            let open Raylib in
            check_collision_circle_rec ball.position (float_of_int ball.radius) (Rectangle.create (Vector2.x block.position) (Vector2.y block.position) (Vector2.x block.dimensions) (Vector2.y block.dimensions))
            
            (*&&
            ((Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) >= (Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) &&
            (Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) >= (Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius)) ||
            ((Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius) >= (Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) &&
            (Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius) >= (Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius))
*)
        let collision_y (ball : Ball.t) (block : Block.t) =
            let open Raylib in
            check_collision_circle_rec ball.position (float_of_int ball.radius) (Rectangle.create (Vector2.x block.position) (Vector2.y block.position) (Vector2.x block.dimensions) (Vector2.y block.dimensions))
            
            (*&&
            ((Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) >= (Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) &&
            (Vector2.y ball.position +. float_of_int ball.radius -. Vector2.y block.position) >= (Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius)) ||
            ((Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius) >= (Vector2.x ball.position +. float_of_int ball.radius -. Vector2.x block.position) &&
            (Vector2.y block.position +. Vector2.y block.dimensions -. Vector2.y ball.position -. float_of_int ball.radius) >= (Vector2.x block.position +. Vector2.x block.dimensions -. Vector2.x ball.position -. float_of_int ball.radius))
*)      
        let draw {paddle; ball; blocks; pause; score; frame_counter} =
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