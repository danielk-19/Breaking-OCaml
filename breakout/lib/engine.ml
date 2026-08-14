(* Engine for Breakout *)
open Raylib
open Types
open Drawing

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
        (match state.difficulty with
        | Easy ->
            state.lives <- easy_lives;
            state.paddle.dimensions <- Vector2.create (paddle_dim_x *. increase_multiplier) (Vector2.y state.paddle.dimensions);
            state.paddle.max_speed <- paddle_speed *. increase_multiplier;
            state.ball.max_speed <- ball_speed *. decrease_multiplier
        | Medium ->
            state.lives <- medium_lives;
            state.paddle.dimensions <- Vector2.create (paddle_dim_x) (Vector2.y state.paddle.dimensions);
            state.paddle.max_speed <- paddle_speed;
            state.ball.max_speed <- ball_speed
        | Hard ->
            state.lives <- hard_lives;
            state.paddle.dimensions <- Vector2.create (paddle_dim_x *. decrease_multiplier) (Vector2.y state.paddle.dimensions);
            state.paddle.max_speed <- paddle_speed *. decrease_multiplier;
            state.ball.max_speed <- ball_speed *. increase_multiplier
        );
        state

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

                if (screen_constraint_met && menu_constraint_met) then Drawer.draw_button button else ();
                
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
    let random_ball_velocity (state : State.t) =
        let random_float = Random.float 1. in
        let vel_x = state.ball.max_speed *. random_float in
        let vel_y = sqrt (state.ball.max_speed ** 2. -. vel_x ** 2.) in

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
        let pos_x = ((float_of_int screen_width) /. 2. -. Vector2.x state.paddle.dimensions /. 2.) in
        let pos_y = ((float_of_int screen_height) -. Vector2.y state.paddle.dimensions /. 2. -. paddle_y_offset) in
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
            let max_speed = paddle_speed in
            let dimensions = Vector2.create paddle_dim_x paddle_dim_y in
            {Paddle.position; velocity; max_speed; dimensions}
        in

        let ball =
            let pos_x = (Vector2.x paddle.position +. Vector2.x paddle.dimensions /. 2.) in
            let pos_y = (Vector2.y paddle.position -. ball_rad -. ball_paddle_y_offset) in

            let position = Vector2.create pos_x pos_y in
            let velocity = Vector2.create 0. 0. in
            let max_speed = ball_speed in
            let radius = (int_of_float ball_rad) in
            {Ball.position; velocity; max_speed; radius}
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
                let vx, vy = random_ball_velocity state in
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
            if (Vector2.x paddle.position -. paddle.max_speed *. rate < 0.) then -.(Vector2.x paddle.position)
            else -.(paddle.max_speed *. rate)
          else if (is_key_down Key.Right &&
            (Vector2.x paddle.position) < ((float_of_int screen_width) -. (Vector2.x paddle.dimensions))) then
                let right_pos = Vector2.x paddle.position +. Vector2.x paddle.dimensions in
                if (right_pos +. paddle.max_speed *. rate > float_of_int screen_width) then (float_of_int screen_width -. right_pos)
                else (paddle.max_speed *. rate)
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
        if (state.level > num_levels) then {state with gameScreen = GameWon}
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
        let open Drawer in
        match state.gameScreen with
        | Start -> start_menu state |> draw_game_menu
        | Pause -> pause_menu state |> draw_game_menu
        | Active | Inactive -> active_screen state
        | GameLost -> game_over_screen state
        | GameWon -> game_winner_screen state
        | Terminate -> state

end