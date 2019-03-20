module Wheel
    open Types

    let values = [Pos0;Pos1;Pos2;Pos3;Pos4;Pos5;Pos6;Pos7;Pos8;Pos9]
    let complements = values |> List.rev
    let withComplements = List.zip values complements

    let inc wheel = 
        let index = values |> List.findIndex (fun v -> v = wheel)
        values.[(index+1) % values.Length]

    let dec wheel = 
        let index = values |> List.findIndex (fun v -> v = wheel)
        values.[(index-1) % values.Length]

    let add wheel1 wheel2 =
        let mutable carry = false
        let mutable result = wheel1
        let mutable addor = wheel2
        while addor <> Pos0 do
            addor <- dec addor
            result <- inc result
            carry <- carry || result = Pos0
        (result, carry)

    let complement wheel = 
        withComplements |> List.find (fun w -> fst w = wheel) |> snd

    let init () =
        Pos0

    let fromChar c = 
        match c with
        | '0' -> Pos0
        | '1' -> Pos1
        | '2' -> Pos2
        | '3' -> Pos3
        | '4' -> Pos4
        | '5' -> Pos5
        | '6' -> Pos6
        | '7' -> Pos7
        | '8' -> Pos8
        | '9' -> Pos9
        | _  -> Pos0

    let toChar w = 
        match w with
        | Pos0 -> '0'
        | Pos1 -> '1'
        | Pos2 -> '2'
        | Pos3 -> '3'
        | Pos4 -> '4'
        | Pos5 -> '5'
        | Pos6 -> '6'
        | Pos7 -> '7'
        | Pos8 -> '8'
        | Pos9 -> '9'
