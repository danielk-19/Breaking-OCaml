(* GUI System for Breakout *)
open Raylib
open Types

(* Drawing Methods *)
module Drawer = struct

    let draw_exit_button (button : Button.t) =
        let dims = Vector2.x button.dimensions in
        let start_pos_x, start_pos_y = (Vector2.x button.position, Vector2.y button.position) in

        let start_pos = Vector2.create start_pos_x start_pos_y in
        let end_pos = Vector2.create (start_pos_x +. dims) (start_pos_y +. dims) in

        draw_line_ex start_pos end_pos exit_button_thickness exit_button_color;
            
        let start_pos = Vector2.create start_pos_x (start_pos_y +. dims) in
        let end_pos = Vector2.create (start_pos_x +. dims) start_pos_y in
            
        draw_line_ex start_pos end_pos exit_button_thickness exit_button_color
    
    let draw_button (button : Button.t) =
        match button.label with
        | "Exit" -> draw_exit_button button
        | _ ->
            let px, py = (Vector2.x button.position, Vector2.y button.position) in
            let dimx, dimy = (Vector2.x button.dimensions, Vector2.y button.dimensions) in
            draw_rectangle_lines_ex (Rectangle.create px py dimx dimy) outline_thickness button_outline_color;

            let text_width = measure_text button.label text_size in
            let button_center_w, button_center_h = (px +. dimx /. 2., py +. dimy /. 2.) in
            let tx, ty = (int_of_float button_center_w - text_width / 2, int_of_float button_center_h - text_size / 2) in
            draw_text button.label tx ty text_size text_color
        
    let settings_menu (state : State.t) =
        draw_rectangle 0 0 screen_width screen_height menu_tint;

        let settings_box = Rectangle.create settings_menu_pos_x settings_menu_pos_y settings_menu_width settings_menu_height in

        draw_rectangle_rounded settings_box menu_roundness menu_smoothness settings_menu_background_color;
        draw_rectangle_rounded_lines_ex settings_box menu_roundness menu_smoothness outline_thickness settings_menu_outline_color;

        let settings_font_size = text_size / 2 in

        let lives_text = "Lives: " ^ string_of_int
            (match state.difficulty with
            | Easy -> easy_lives
            | Medium -> medium_lives
            | Hard -> hard_lives)
        in
        let lives_width = measure_text lives_text settings_font_size in
        draw_text lives_text
            (int_of_float (settings_menu_pos_x +. (settings_menu_width -. float_of_int lives_width) /. 2.))
            (int_of_float (settings_menu_pos_y +. difficulty_button_y_offset +. difficulty_button_height +. settings_spacing_offset))
            settings_font_size text_color;

        let ball_text = "Ball Speed: " ^ string_of_float state.ball.max_speed in
        let ball_text_width = measure_text ball_text settings_font_size in
        draw_text ball_text
            (int_of_float (settings_menu_pos_x +. (settings_menu_width -. float_of_int ball_text_width) /. 2.))
            (int_of_float (settings_menu_pos_y +. difficulty_button_y_offset +. difficulty_button_height +. float_of_int settings_font_size +. 2. *. settings_spacing_offset))
            settings_font_size text_color;

        let paddle_text = "Paddle Length/Speed: " ^ string_of_float (Vector2.x state.paddle.dimensions) ^ " | " ^ string_of_float state.paddle.max_speed in
        let paddle_text_width = measure_text paddle_text settings_font_size in
        draw_text paddle_text
            (int_of_float (settings_menu_pos_x +. (settings_menu_width -. float_of_int paddle_text_width) /. 2.))
            (int_of_float (settings_menu_pos_y +. difficulty_button_y_offset +. difficulty_button_height +. float_of_int (settings_font_size * 2) +. 3. *. settings_spacing_offset))
            settings_font_size text_color;

        state

    let draw_game_menu (state : State.t) =
        match state.gameMenu with
        | Main -> state
        | Settings -> settings_menu state

    let start_menu (state : State.t) =
        clear_background starting_screen_color;

        let text = "Breaking OCaml" in
        let font_size = text_size * 4 / 3 in
        let font_width = (measure_text text font_size) in
        let font_x = (screen_width - font_width) / 2 in
        let font_y = screen_height / 8 in
        draw_text text
            font_x font_y
            font_size text_color;

        state

    let draw_gameplay (state : State.t) =
        clear_background background_color;

        let paddle_rec = Rectangle.create (Vector2.x state.paddle.position) (Vector2.y state.paddle.position) (Vector2.x state.paddle.dimensions) (Vector2.y state.paddle.dimensions) in
        draw_rectangle_rounded paddle_rec paddle_roundness paddle_smoothness paddle_color;
        draw_circle_v state.ball.position (float_of_int state.ball.radius) ball_color;
            
        List.iter (fun (block : Block.t) ->
            let block_rec = Rectangle.create (Vector2.x block.position) (Vector2.y block.position) (Vector2.x block.dimensions) (Vector2.y block.dimensions) in
            draw_rectangle_rounded block_rec block_roundness block_smoothness block.color;
            draw_rectangle_rounded_lines_ex block_rec block_roundness block_smoothness outline_thickness block_outline_color
        ) state.blocks;
        
        let text = Printf.sprintf "%05d" state.score in
        let font_width = (measure_text text text_size) in
        draw_text text
            ((screen_width - font_width) / 2) text_corner_offset
            text_size text_color;
            
        let rec draw_lives (total_lives : int) (curr_lives : int) (acc : int) =
            if (acc > total_lives) then ()
            else if (acc > curr_lives) then begin
                draw_circle_lines (text_corner_offset + display_lives_radius * (2 * acc - 1) + standard_spacing_offset * (acc - 1)) (text_corner_offset + display_lives_radius) (float_of_int display_lives_radius) ball_color;
                draw_lives total_lives curr_lives (acc + 1)
            end else begin
                draw_circle (text_corner_offset + display_lives_radius * (2 * acc - 1) + standard_spacing_offset * (acc - 1)) (text_corner_offset + display_lives_radius) (float_of_int display_lives_radius) ball_color;
                draw_lives total_lives curr_lives (acc + 1)
            end
        in

        draw_lives (match state.difficulty with
            | Easy -> easy_lives
            | Medium -> medium_lives
            | Hard -> hard_lives) state.lives 1;

        let text = "Level " ^ string_of_int state.level in
        draw_text text text_corner_offset (screen_height - (text_corner_offset + text_size) / 2) (text_size / 2) text_color

    let active_screen (state : State.t) =
        draw_gameplay state;
            
        state

    let pause_menu (state : State.t) =
        draw_gameplay state;

        draw_rectangle 0 0 screen_width screen_height menu_tint;

        let text = "Press Up Arrow To Continue" in
        let font_width = (measure_text text text_size) in
        let font_x = (screen_width - font_width) / 2 in
        let font_y = screen_height / 3 in
        draw_text text
            font_x font_y
            text_size text_color;
            
        state

    let game_winner_screen (state : State.t) =
        clear_background ending_screen_color;

        let winner_text = "Winner!" in
        let winner_width = (measure_text winner_text text_size) in            
        draw_text winner_text
            ((screen_width - winner_width) / 2) (screen_height / 2 - text_size)
            text_size text_color;

        let completion_text = Printf.sprintf "Levels Cleared: %02d Score: %05d" (state.level - 1) state.score in
        let completion_width = (measure_text completion_text text_size) in            
        draw_text completion_text
            ((screen_width - completion_width) / 2) (screen_height / 2)
            text_size text_color;

        state

    let game_over_screen (state : State.t) =
        clear_background ending_screen_color;

        let game_over_text = "Game Over!" in
        let game_over_width = (measure_text game_over_text text_size) in
        draw_text game_over_text
            ((screen_width - game_over_width) / 2) (screen_height / 2 - text_size)
            text_size text_color;

        let score_text = Printf.sprintf "Score: %05d" state.score in
        let score_width = (measure_text score_text text_size) in
        draw_text score_text
            ((screen_width - score_width) / 2) (screen_height / 2)
            text_size text_color;

        state

end