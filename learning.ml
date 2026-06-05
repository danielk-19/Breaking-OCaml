(* Huh this is a weird language *)

let calculator (num1: int) (num2: int) : string =
    let product : int list =
        if (num2 = 0) then -1::[] else (num1 / num2)::[] in
    begin match product with
    | s::e -> "something"
    | [] -> "nothing"
    end;;

print_string (calculator 5 0);;