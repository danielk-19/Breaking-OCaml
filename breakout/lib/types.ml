(* Defines All Types and Constants *)
open Raylib

(* Constants *)
let screen_width = 960
let screen_height = 720
let num_levels = 5
let points_per_block = 50

(* Difficulty Settings *)
let easy_lives = 5
let medium_lives = 3
let hard_lives = 1
let display_lives_radius = 20
let increase_multiplier = 1.25
let decrease_multiplier = 0.75

(* Drawing *)
let background_color = Color.create 5 0 20 255
let starting_screen_color = Color.black
let ending_screen_color = Color.black
let text_color = Color.white
let text_size = 48
let text_corner_offset = 25
let standard_spacing_offset = 10

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
let block_outline_color = Color.create 30 0 60 164

(* Menu *)
let menu_tint = Color.create 0 0 0 128
let outline_thickness = 2.
let menu_roundness = 0.25
let menu_smoothness = 25
let settings_menu_width = float_of_int screen_width *. 0.75
let settings_menu_height = float_of_int screen_height *. 0.75
let settings_menu_pos_x = (float_of_int screen_width -. settings_menu_width) /. 2.
let settings_menu_pos_y = (float_of_int screen_height -. settings_menu_height) /. 2.
let settings_menu_background_color = Color.create 15 10 30 255
let settings_menu_outline_color = Color.create 50 20 80 255
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
            mutable max_speed : float;
            radius : int;
        }
    end

module Paddle =
    struct
        type t = {
            mutable position : Vector2.t;
            mutable velocity : float;
            mutable max_speed : float;
            mutable dimensions : Vector2.t;
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