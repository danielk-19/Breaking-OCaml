(* Basic Breakout Implementation *)

let screen_width = 960;;
let screen_height = 720;;

let setup () =
  Raylib.init_window screen_width screen_height "Breakout";
  Raylib.set_target_fps 60;;

let loop () =
  let open Raylib in

  let dim_x = 300 in
  let dim_y = 50 in
  let pos_x = ref (screen_width / 2 - dim_x / 2) in
  let pos_y = (screen_height - dim_y / 2 - 50) in

  let text = "Press Up Arrow To Begin" in
  let font_size = 36 in
  let font_width = (measure_text text font_size) in

  let start = ref false in

  while not (window_should_close ()) do
    
    start := if (is_key_pressed Key.Up) then true else !start;

    begin_drawing ();
      clear_background Color.darkgray;
      draw_rectangle !pos_x pos_y dim_x dim_y Color.white;
      draw_circle (screen_width / 2 - 5) (screen_height / 2 - 5) 10.0 Color.white;
      if !start then () else
        draw_text text
        (screen_width / 2 - font_width / 2) (screen_height / 4) font_size Color.white;
    end_drawing ();

    begin match !start with
    | false -> ()
    | true ->
      pos_x := if (is_key_pressed_repeat Key.Left && (!pos_x > 0)) then (!pos_x - 15) else !pos_x;
      pos_x := if (is_key_pressed_repeat Key.Right && (!pos_x < (screen_width - dim_x))) then (!pos_x + 15) else !pos_x;
    end
  done;
  close_window();;
  
let () = setup () |> loop;;