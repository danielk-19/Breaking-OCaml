(* Implemented Modules for Breakout *)
open Raylib

(* Constants *)
let screen_width = 960
let screen_height = 720
let points_per_block = 50

(* Difficulty Settings *)
let easy_lives = 5
let medium_lives = 3
let hard_lives = 1

(* Drawing *)
let background_color = Color.create 5 0 20 255
let starting_screen_color = Color.black
let ending_screen_color = Color.black
let text_color = Color.white
let text_size = 48
let text_corner_offset = 25

(* Paddle *)
let paddle_dim_x = float_of_int screen_width *. 0.2
let paddle_dim_y = 20.
let paddle_y_offset = 50.
let paddle_speed = 800.
let paddle_roundness = 1.
let paddle_smoothness = 50
let paddle_color = Color.white

(* Ball *)
let ball_rad = 10.
let ball_speed = 850.
let ball_color = Color.white
let ball_paddle_y_offset = 50.
let ball_reset_cutoff = 75.

(* Blocks *)
let block_dim_x = 100.
let block_dim_y = 50.
let num_blocks_x = 8
let num_blocks_y = 4
let block_offset = 5.
let block_colors = [
    Color.create 9 33 130 255;
    Color.create 130 9 21 255;
    Color.create 158 134 11 255;
    Color.create 7 94 29 255
]
let block_initial_y = 100.
let block_roundness = 0.3
let block_smoothness = 30
let block_outline_color = Color.create 21 5 54 128

(* Menu *)
let menu_tint = Color.create 0 0 0 128
let outline_thickness = 3.
let menu_roundness = 0.25
let menu_smoothness = 25
let settings_menu_width = float_of_int screen_width *. 0.75
let settings_menu_height = float_of_int screen_height *. 0.75
let settings_menu_pos_x = (float_of_int screen_width -. settings_menu_width) /. 2.
let settings_menu_pos_y = (float_of_int screen_height -. settings_menu_height) /. 2.
let settings_menu_background_color = Color.create 28 9 135 255
let settings_menu_outline_color = Color.create 9 2 46 255
let settings_spacing_offset = 10.

(* Buttons *)
let button_outline_color = Color.white

(* Exit Button *)
let exit_button_dims = 50.
let exit_button_corner_offset = 25.
let exit_button_thickness = 10.
let exit_button_color = Color.white

(* Settings Button *)
let settings_button_height = 100.
let settings_button_width = 400.

(* Start Button *)
let start_button_height = 100.
let start_button_width = 400.

(* Difficulty Button *)
let difficulty_button_height = settings_menu_height *. 0.2
let difficulty_button_width = settings_menu_width *. 0.6
let difficulty_button_y_offset = 50.

(* Custom Types *)
type gameState = 
    | Start
    | Active
    | Inactive
    | Pause
    | GameLost
    | GameWon
    | Terminate

type menuState =
    | Main
    | Settings

type gameDifficulty =
    | Easy
    | Medium
    | Hard

type buttonAction =
    | GameStart
    | GameSettings
    | GameExit
    | ChangeDifficulty

module Ball =
    struct
        type t = {
            mutable position : Vector2.t;
            mutable velocity : Vector2.t;
            radius : int;
        }
    end

module Paddle =
    struct
        type t = {
            mutable position : Vector2.t;
            mutable velocity : float;
            dimensions : Vector2.t;
        }
    end

module Block =
    struct
        type t = {
            position : Vector2.t;
            dimensions : Vector2.t;
            color : Color.t;
        }
    end

module Button =
    struct
        type t = {
            position : Vector2.t;
            dimensions : Vector2.t;
            mutable label : string;
            action : buttonAction;
            screen_constraints : gameState list;
            menu_constraints : menuState list;
        }
    end

module State =
    struct
        type t = {
            paddle : Paddle.t;
            ball : Ball.t;
            mutable blocks : Block.t list;
            mutable gameScreen : gameState;
            mutable gameMenu : menuState;
            mutable difficulty : gameDifficulty;
            mutable score : int;
            mutable lives : int;
            buttons : Button.t list;
            mutable pressing_button : bool;
            mutable level : int;
        }
    end

(* Drawing Methods *)
module Drawing = struct

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

        let lives_text = "Lives: " ^ string_of_int state.lives in
        let lives_width = measure_text lives_text settings_font_size in
        draw_text lives_text
            (int_of_float (settings_menu_pos_x +. (settings_menu_width -. float_of_int lives_width) /. 2.))
            (int_of_float (settings_menu_pos_y +. difficulty_button_y_offset +. difficulty_button_height +. settings_spacing_offset))
            settings_font_size text_color;

        let ball_text = "Ball Speed: " ^ string_of_float ball_speed in
        let ball_text_width = measure_text ball_text settings_font_size in
        draw_text ball_text
            (int_of_float (settings_menu_pos_x +. (settings_menu_width -. float_of_int ball_text_width) /. 2.))
            (int_of_float (settings_menu_pos_y +. difficulty_button_y_offset +. difficulty_button_height +. float_of_int settings_font_size +. 2. *. settings_spacing_offset))
            settings_font_size text_color;

        let paddle_text = "Paddle Length/Speed: " ^ string_of_float (Vector2.x state.paddle.dimensions) ^ " | " ^ string_of_float paddle_speed in
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
        
        let text = Printf.sprintf "Score: %04d" state.score in
        let font_size = text_size / 2 in
        let font_width = (measure_text text font_size) in
        draw_text text
            ((screen_width - font_width) / 2) text_corner_offset
            font_size text_color;
            
        let text = "Lives: " ^ string_of_int state.lives in
        draw_text text text_corner_offset text_corner_offset font_size text_color;

        let text = "Level: " ^ string_of_int state.level in
        draw_text text text_corner_offset (screen_height - text_corner_offset - font_size) font_size text_color

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

        let completion_text = Printf.sprintf "Levels Cleared: %02d Score: %04d" (state.level - 1) state.score in
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

        let score_text = Printf.sprintf "Score: %04d" state.score in
        let score_width = (measure_text score_text text_size) in
        draw_text score_text
            ((screen_width - score_width) / 2) (screen_height / 2)
            text_size text_color;

        state

end

(* Utility Methods *)
module Utility = struct

    (* Handle Difficulty *)
    let update_button_label (state : State.t) (target_button : Button.t) (new_label : string) =
        {state with buttons =
            List.map (fun (button : Button.t) ->
                if (button.action = target_button.action) then {button with label = new_label}
                else button
            ) state.buttons
        }

    let change_difficulty (state : State.t) =
        match state.difficulty with
        | Easy -> {state with lives = easy_lives}
        | Medium -> {state with lives = medium_lives}
        | Hard -> {state with lives = hard_lives}

    let difficulty_handling (state : State.t) (button : Button.t) =
        (match state.difficulty with
        | Easy -> update_button_label {state with difficulty = Medium} button "Medium"
        | Medium -> update_button_label {state with difficulty = Hard} button "Hard"
        | Hard -> update_button_label {state with difficulty = Easy} button "Easy")
        |> change_difficulty
    
    (* Button Handling *)
    let button_pressed (bounds : Rectangle.t) =
        (check_collision_point_rec (get_mouse_position ()) bounds &&
        is_mouse_button_pressed MouseButton.Left)
    
    let rec button_screen_constraint_check (screen : gameState) (constraints : gameState list) =
        match constraints with
        | [] -> false
        | cons :: rest -> (screen = cons) || button_screen_constraint_check screen rest
    
    let rec button_menu_constraint_check (menu : menuState) (constraints : menuState list) =
        match constraints with
        | [] -> false
        | cons :: rest -> (menu = cons) || button_menu_constraint_check menu rest

    let rec button_handling (state : State.t) =
        button_handling_rec state state.buttons
        
        and button_handling_rec (state : State.t) buttons =
            match buttons with
            | [] -> {state with pressing_button = false}
            | button :: rest ->
                let pos_x, pos_y = (Vector2.x button.position, Vector2.y button.position) in
                let dim_x, dim_y = (Vector2.x button.dimensions, Vector2.y button.dimensions) in
                let bounds = Rectangle.create pos_x pos_y dim_x dim_y in
                let screen_constraint_met = List.is_empty button.screen_constraints || button_screen_constraint_check state.gameScreen button.screen_constraints in
                let menu_constraint_met = List.is_empty button.menu_constraints || button_menu_constraint_check state.gameMenu button.menu_constraints in

                if (screen_constraint_met && menu_constraint_met) then Drawing.draw_button button else ();
                
                if (screen_constraint_met && menu_constraint_met && button_pressed bounds && not state.pressing_button) then
                    match button.action with
                    | GameStart -> {state with gameScreen = Inactive; pressing_button = true}
                    | GameSettings -> {state with gameMenu = Settings; pressing_button = true}
                    | GameExit -> {state with gameScreen = Terminate; pressing_button = true}
                    | ChangeDifficulty -> {(difficulty_handling state button) with pressing_button = true}
                else button_handling_rec state rest

    (* Collision Checking *)
    let wall_collision_x (state : State.t) =
        let ball = state.ball in
        (Vector2.x ball.position <= float_of_int ball.radius ||
        Vector2.x ball.position >= float_of_int (screen_width - ball.radius))

    let wall_collision_y (state : State.t) =
        let ball = state.ball in
        (Vector2.y ball.position <= float_of_int ball.radius)

    let ball_reset_required (state : State.t) =
        let ball, paddle = (state.ball, state.paddle) in
        ((Vector2.y ball.position -. float_of_int ball.radius) >= Vector2.y paddle.position +. ball_reset_cutoff)

    let block_collision (state : State.t) (block : Block.t) =
        let ball = state.ball in
        check_collision_circle_rec ball.position (float_of_int ball.radius) (Rectangle.create (Vector2.x block.position) (Vector2.y block.position) (Vector2.x block.dimensions) (Vector2.y block.dimensions))
        
    let paddle_collision (state : State.t) =
        let ball, paddle = (state.ball, state.paddle) in
        check_collision_circle_rec ball.position (float_of_int ball.radius) (Rectangle.create (Vector2.x paddle.position) (Vector2.y paddle.position) (Vector2.x paddle.dimensions) (Vector2.y paddle.dimensions))
        
    let paddle_hit (state : State.t) =
        let ball, paddle = (state.ball, state.paddle) in
        let hit = (((Vector2.x ball.position -. Vector2.x paddle.position) /. Vector2.x paddle.dimensions) -. 0.5) *. 2.0 in
        Float.max (-1.) (Float.min hit 1.) *. 1.0472

    (* Overlap Calculation *)
    let standard_overlap_direction (left_overlap : float) (right_overlap : float) (top_overlap : float) (bottom_overlap : float) =
        let min_x = Float.min left_overlap right_overlap in
        let min_y = Float.min top_overlap bottom_overlap in

        if (min_x < min_y) then
                if (Float.compare left_overlap right_overlap < 0) then (-.(left_overlap), 0.0)
                else (right_overlap, 0.0)
            else
                if (Float.compare top_overlap bottom_overlap < 0) then (0.0, -.(top_overlap))
                else (0.0, bottom_overlap)

    let wall_overlap_direction (left_overlap : float) (right_overlap : float) (top_overlap : float) =
        let min_x = Float.min left_overlap right_overlap in
        let min_y = top_overlap in

        if (min_x < min_y) then
                if (Float.compare left_overlap right_overlap < 0) then (left_overlap, 0.0)
                else (-.right_overlap, 0.0)
            else
                (0.0, top_overlap)

    let ball_overlap (state : State.t) (position : Vector2.t) (dimensions : Vector2.t) =
        let b_x = Vector2.x state.ball.position in 
        let b_y = Vector2.y state.ball.position in
        let p_x = Vector2.x position in 
        let p_y = Vector2.y position in 
        let p_w = Vector2.x dimensions in 
        let p_h = Vector2.y dimensions in 
        let radius = float_of_int state.ball.radius in

        let left_overlap = (b_x +. radius) -. p_x in
        let right_overlap = (p_x +. p_w) -. (b_x -. radius) in
        let top_overlap = (b_y +. radius) -. p_y in
        let bottom_overlap = (p_y +. p_h) -. (b_y -. radius) in

        let x_diff, y_diff =
            if (p_w *. p_h = float_of_int (screen_width * screen_height)) then
                wall_overlap_direction left_overlap right_overlap top_overlap
            else
                standard_overlap_direction left_overlap right_overlap top_overlap bottom_overlap
        in

        Vector2.create (b_x +. x_diff) (b_y +. y_diff)

    (* Velocity Collision Handling *)
    let ball_wall_collision_velocity (state : State.t) =
        let ball = state.ball in
        let vx =
            if (wall_collision_x state) then
                -.(Vector2.x ball.velocity)
            else (Vector2.x ball.velocity)
        in

        let vy =
            if (wall_collision_y state) then
                -.(Vector2.y ball.velocity)
            else (Vector2.y ball.velocity)
        in

        let position = Vector2.create 0. 0. in
        let dimensions = Vector2.create (float_of_int screen_width) (float_of_int screen_height) in

        state.ball.position <- ball_overlap state position dimensions;

        state.ball.velocity <- Vector2.create vx vy;

        state

    let ball_paddle_collision_velocity (state : State.t) =
        let ball = state.ball in
        let paddle = state.paddle in

        let speed = sqrt ((Vector2.x ball.velocity ** 2.) +. (Vector2.y ball.velocity ** 2.)) in

        state.ball.position <- ball_overlap state paddle.position paddle.dimensions;

        let angle = (paddle_hit state) in

        let vx = speed *. sin angle in
        let vy = speed *. cos angle in

        let direction_x = if (Float.sign_bit (Vector2.x ball.velocity) <> Float.sign_bit vx) then -1. else 1. in
        let direction_y = if (Vector2.y ball.position < (Vector2.y paddle.position +. Vector2.y paddle.dimensions /. 2.) && vy = abs_float vy) then -1. else 1. in

        state.ball.velocity <- Vector2.create (vx *. direction_x) (vy *. direction_y);

        state
            
    let ball_block_collision_velocity (state : State.t) (block : Block.t) =
        let ball = state.ball in

        let vx = Vector2.x ball.velocity in
        let vy = Vector2.y ball.velocity in

        let left_overlap = abs_float (Vector2.x block.position -. (Vector2.x ball.position +. (float_of_int ball.radius))) in
        let right_overlap = abs_float ((Vector2.x block.position +. Vector2.x block.dimensions) -. (Vector2.x ball.position -. (float_of_int ball.radius))) in
        let top_overlap = abs_float (Vector2.y block.position -. (Vector2.y ball.position +. (float_of_int ball.radius))) in
        let bottom_overlap = abs_float ((Vector2.y block.position +. Vector2.y block.dimensions) -. (Vector2.y ball.position -. (float_of_int ball.radius))) in

        let min_overlap = Float.min (Float.min left_overlap right_overlap) (Float.min top_overlap bottom_overlap) in

        let dir_x, dir_y =
            if (min_overlap = left_overlap || min_overlap = right_overlap) then (-1., 1.)
            else (1., -1.)
        in

        state.ball.position <- ball_overlap state block.position block.dimensions;

        state.ball.velocity <- Vector2.create (vx *. dir_x) (vy *. dir_y);

        state

    let rec check_block_collision (state : State.t) =
        check_block_collision_list state state.blocks

        and check_block_collision_list (state : State.t) blocks =
            match blocks with
            | [] -> None
            | block :: rest ->
                if block_collision state block then Some block
            else
                check_block_collision_list state rest

    let collision_handling (state : State.t) =
        if (paddle_collision state) then ball_paddle_collision_velocity state
        else if (wall_collision_x state || wall_collision_y state) then ball_wall_collision_velocity state
        else
            match (check_block_collision state) with
            | None -> state
            | Some block ->
                state.blocks <- List.filter (fun (b : Block.t) -> b <> block) state.blocks;
                ball_block_collision_velocity state block
    
    (* Game State Setup *)
    let random_ball_velocity () =
        let random_float = Random.float 1. in
        let vel_x = ball_speed *. random_float in
        let vel_y = sqrt (ball_speed ** 2. -. vel_x ** 2.) in

        let random_num = Random.int 2 in
        let dir_x, dir_y =
            if (random_num = 0) then (1., -1.) else (-1., -1.)
        in

        (vel_x *. dir_x, vel_y *. dir_y)

    let rec make_blocks (x : float) (y : float) (acc : Block.t list) (color : Color.t list) =
        let max_y = (block_dim_y *. float_of_int num_blocks_y +. block_offset *. float_of_int num_blocks_y +. block_initial_y) in
        let max_x = (block_dim_x *. float_of_int num_blocks_x +. block_offset *. float_of_int num_blocks_x +. (float_of_int screen_width -. (block_dim_x *. float_of_int num_blocks_x +. block_offset *. float_of_int num_blocks_x)) /. 2.) in
        if y >= max_y then acc
        else if x >= max_x then
            make_blocks ((float_of_int screen_width -. (block_dim_x *. float_of_int num_blocks_x +. block_offset *. float_of_int num_blocks_x)) /. 2.) (y +. block_dim_y +. block_offset) acc (List.tl color)
        else
            let block = {Block.position = Vector2.create x y; dimensions = Vector2.create block_dim_x block_dim_y; color = List.hd color} in
            make_blocks (x +. block_dim_x +. block_offset) y (block :: acc) color

    let reset (state : State.t) =
        let pos_x = ((float_of_int screen_width) /. 2. -. paddle_dim_x /. 2.) in
        let pos_y = ((float_of_int screen_height) -. paddle_dim_y /. 2. -. paddle_y_offset) in
        state.paddle.position <- Vector2.create pos_x pos_y;
        state.paddle.velocity <- 0.;
        state.ball.velocity <- Vector2.create 0. 0.;
        state.ball.position <- Vector2.create (Vector2.x state.paddle.position +. Vector2.x state.paddle.dimensions /. 2.) (Vector2.y state.paddle.position -. ball_rad -. ball_paddle_y_offset);
        state.blocks <- make_blocks ((float_of_int screen_width -. (block_dim_x *. float_of_int num_blocks_x +. block_offset *. float_of_int num_blocks_x)) /. 2.) block_initial_y [] block_colors;
        state.gameScreen <- Inactive;
        state

    let start () =
        let paddle = 
            let pos_x = ((float_of_int screen_width) /. 2. -. paddle_dim_x /. 2.) in
            let pos_y = ((float_of_int screen_height) -. paddle_dim_y /. 2. -. paddle_y_offset) in  

            let position = Vector2.create pos_x pos_y in
            let velocity = 0. in
            let dimensions = Vector2.create paddle_dim_x paddle_dim_y in
            {Paddle.position; velocity; dimensions}
        in

        let ball =
            let pos_x = (Vector2.x paddle.position +. Vector2.x paddle.dimensions /. 2.) in
            let pos_y = (Vector2.y paddle.position -. ball_rad -. ball_paddle_y_offset) in

            let position = Vector2.create pos_x pos_y in
            let velocity = Vector2.create 0. 0. in
            let radius = (int_of_float ball_rad) in
            {Ball.position; velocity; radius}
        in

        let blocks =
            make_blocks ((float_of_int screen_width -. (block_dim_x *. float_of_int num_blocks_x +. block_offset *. float_of_int num_blocks_x)) /. 2.) block_initial_y [] block_colors
        in

        let buttons =
            let exit_button =
                let position = Vector2.create (float_of_int screen_width -. exit_button_corner_offset -. exit_button_dims) exit_button_corner_offset in
                let dimensions = Vector2.create exit_button_dims exit_button_dims in
                let label = "Exit" in
                let action = GameExit in
                let screen_constraints = [] in
                let menu_constraints = [] in

                {Button.position; dimensions; label; action; screen_constraints; menu_constraints}
            in

            let settings_button =
                let position = Vector2.create ((float_of_int screen_width -. settings_button_width) /. 2.) ((float_of_int screen_height -. settings_button_height) /. 2. +. start_button_height) in
                let dimensions = Vector2.create settings_button_width settings_button_height in
                let label = "Settings" in
                let action = GameSettings in
                let screen_constraints = [Start; Pause] in
                let menu_constraints = [Main] in

                {Button.position; dimensions; label; action; screen_constraints; menu_constraints}
            in

            let start_button =
                let position = Vector2.create ((float_of_int screen_width -. start_button_width) /. 2.) ((float_of_int screen_height -. start_button_height) /. 2.) in
                let dimensions = Vector2.create start_button_width start_button_height in
                let label = "Start" in
                let action = GameStart in
                let screen_constraints = [Start] in
                let menu_constraints = [Main] in

                {Button.position; dimensions; label; action; screen_constraints; menu_constraints}
            in

            let difficulty_button =
                let position = Vector2.create (settings_menu_pos_x +. (settings_menu_width -. difficulty_button_width) /. 2.) (settings_menu_pos_y +. difficulty_button_y_offset) in
                let dimensions = Vector2.create difficulty_button_width difficulty_button_height in
                let label = "Medium" in
                let action = ChangeDifficulty in
                let screen_constraints = [Start; Pause] in
                let menu_constraints = [Settings] in

                {Button.position; dimensions; label; action; screen_constraints; menu_constraints}
            in

            [exit_button; settings_button; start_button; difficulty_button]
        in

        {State.paddle; ball; blocks; gameScreen = Start; gameMenu = Main; difficulty = Medium; score = 0; lives = medium_lives; buttons; pressing_button = false; level = 1}
    
    (* Control Handling *)
    let menu_control_handling (state : State.t) =
        match state.gameMenu with
        | Main -> button_handling state
        | Settings ->
            let menu_bounds = Rectangle.create settings_menu_pos_x settings_menu_pos_y settings_menu_width settings_menu_height in
            if (is_mouse_button_pressed MouseButton.Left && not (button_pressed menu_bounds)) then {state with gameMenu = Main}
            else button_handling state

    let inactive_state_handler (state : State.t) =
        match state.gameScreen with
        | Inactive ->
            if (is_key_pressed Key.Up) then
                let vx, vy = random_ball_velocity () in
                state.ball.velocity <- Vector2.create vx vy;
                {state with gameScreen = Active}
            else
                let mouse_pos_x =
                    if (get_mouse_x () - int_of_float (Vector2.x state.paddle.dimensions /. 2.) < 0) then Vector2.x state.paddle.dimensions /. 2.
                    else if (get_mouse_x () > screen_width - int_of_float (Vector2.x state.paddle.dimensions /. 2.)) then float_of_int screen_width -. Vector2.x state.paddle.dimensions /. 2.
                    else float_of_int (get_mouse_x ())
                in
                state.paddle.position <- Vector2.create (mouse_pos_x -. Vector2.x state.paddle.dimensions /. 2.) (Vector2.y state.paddle.position);
                state.ball.velocity <- Vector2.create 0. 0.;
                state.ball.position <- Vector2.create (Vector2.x state.paddle.position +. Vector2.x state.paddle.dimensions /. 2.) (Vector2.y state.paddle.position -. ball_rad -. ball_paddle_y_offset);
                state
        | _ -> state

    let control_handling (state : State.t) =
        match state.gameScreen with
        | Start | Pause ->
            if (state.gameScreen = Pause && is_key_pressed Key.Escape) then {state with gameScreen = Start}
            else if (state.gameScreen = Pause && is_key_pressed Key.Up) then {state with gameScreen = if (Vector2.x state.ball.velocity = 0. && Vector2.y state.ball.velocity = 0.) then Inactive else Active}
            else menu_control_handling state
        | Active | Inactive | GameLost | GameWon ->
            if (is_key_pressed Key.R) then reset {state with score = 0; level = 1} |> change_difficulty
            else if ((state.gameScreen = Active || state.gameScreen = Inactive) && is_key_pressed Key.S) then {state with blocks = [List.hd state.blocks]} (* Testing Level Skip *)
            else if ((state.gameScreen = Active || state.gameScreen = Inactive) && is_key_pressed Key.Escape) then {state with gameScreen = Pause}
            else button_handling state |> inactive_state_handler
        | Terminate -> state

    (* Paddle Movement *)
    let paddle_move (state : State.t) =
        let rate = (1. /. float_of_int (get_fps ())) in
        let paddle = state.paddle in
        let velocity =
          if (is_key_down Key.Left && (Vector2.x paddle.position) > 0.) then 
            if (Vector2.x paddle.position -. paddle_speed *. rate < 0.) then -.(Vector2.x paddle.position)
            else -.(paddle_speed *. rate)
          else if (is_key_down Key.Right &&
            (Vector2.x paddle.position) < ((float_of_int screen_width) -. (Vector2.x paddle.dimensions))) then
                let right_pos = Vector2.x paddle.position +. Vector2.x paddle.dimensions in
                if (right_pos +. paddle_speed *. rate > float_of_int screen_width) then (float_of_int screen_width -. right_pos)
                else (paddle_speed *. rate)
          else 0.
        in

        state.paddle.velocity <- velocity;

        state
    
    let paddle_update (state : State.t) =
        state.paddle.position <- Vector2.create (Vector2.x state.paddle.position +. state.paddle.velocity) (Vector2.y state.paddle.position);
        state
    
    (* Ball Movement *)
    let handle_ball_reset (state : State.t) =
        state.lives <- state.lives - 1;
        state.gameScreen <- Inactive

    let ball_update (state : State.t) =
        let rate = (1. /. float_of_int (get_fps ())) in
        if (ball_reset_required state) then handle_ball_reset state
        else state.ball.position <- Vector2.create (Vector2.x state.ball.position +. Vector2.x state.ball.velocity *. rate) (Vector2.y state.ball.position +. Vector2.y state.ball.velocity *. rate);
        state

    (* Game State Handling *)
    let check_win_lose (state : State.t) =
        if (state.level > 3) then {state with gameScreen = GameWon}
        else if (state.lives = 0) then {state with gameScreen = GameLost}
        else state

    let score_update (state : State.t) =
        state.score <- (num_blocks_x * num_blocks_y * state.level - List.length state.blocks) * points_per_block;
        if (List.is_empty state.blocks) then begin
            state.level <- state.level + 1;
            reset state
        end else state

    let game_state_adjuster (state : State.t) =
        match state.gameScreen with
        | Start | Pause -> state
        | _ -> {state with gameMenu = Main}
    
    let game_state_handler (state : State.t) =
        let open Drawing in
        match state.gameScreen with
        | Start -> start_menu state |> draw_game_menu
        | Pause -> pause_menu state |> draw_game_menu
        | Active | Inactive -> active_screen state
        | GameLost -> game_over_screen state
        | GameWon -> game_winner_screen state
        | Terminate -> state

end