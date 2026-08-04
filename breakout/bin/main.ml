(* Basic Breakout Implementation *)
open Raylib
open Breakout_tools
open Objects

let setup () =
  init_window screen_width screen_height "Breakout";
  set_target_fps (get_monitor_refresh_rate 0);
  set_exit_key Key.Null;

  Utility.reset ()

let rec loop (state : State.t) =
  match (window_should_close () || state.gameScreen = Terminate) with
  | true -> close_window ()
  | false ->
    let open Utility in

    begin_drawing ();

    let state =
      state |>
      game_state_handler |>
      check_win_lose |>
      button_handling |>
      control_handling |>

      (fun (state : State.t) ->
        if (state.gameScreen = Active) then
          state |>
          paddle_move |>
          paddle_update |>
          collision_handling |>
          ball_update
        else state
      ) |>

      score_update |>
      game_state_adjuster
    in

    end_drawing ();

    loop state
  
let () = setup () |> loop