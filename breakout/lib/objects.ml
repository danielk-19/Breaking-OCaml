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

module State =
    struct
        type t = {
            paddle : Paddle.t;
            ball : Ball.t;
            pause : bool;
            frames_counter: int
        }
        let draw {paddle; ball; pause; frames_counter} =
            let open Raylib in
            begin_drawing ();

            clear_background Color.darkgray;
            draw_rectangle_v paddle.position paddle.dimensions Color.white;
            draw_circle_v ball.position (float_of_int ball.radius) Color.white;

            if pause then
                let text = "Press Up Arrow To Continue" in
                let font_size = 36 in
                let font_width = (measure_text text font_size) in
                draw_text text
                    (get_screen_width () / 2 - font_width / 2) (get_screen_height () / 4)
                    font_size Color.white;
            else ();

            end_drawing ()
    end