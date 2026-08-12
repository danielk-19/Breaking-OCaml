(* Basic Breakout Implementation *)
open Raylib
open Breakout_tools
open Objects

let setup () =
  init_window screen_width screen_height "Breaking OCaml: A Breakout Game";
  set_target_fps (get_monitor_refresh_rate 0);
  set_exit_key Key.Null;

  Utility.start ()

let rec loop (state : State.t) =
  match (window_should_close () || state.gameScreen = Terminate) with
  | true -> close_window ()
  | false ->
    let open Utility in

    begin_drawing ();

    let state =
      state |>
      check_win_lose |>
      game_state_handler |>
      button_handling |>

      (fun (state : State.t) ->
        if (state.gameScreen = Active || state.gameScreen = Inactive) then
          state |>
          paddle_move |>
          paddle_update |>
          collision_handling |>
          ball_update
        else state
      ) |>

      control_handling |>
      score_update |>
      game_state_adjuster
    in

    end_drawing ();

    loop state

let () = setup () |> loop