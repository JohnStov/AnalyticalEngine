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

    let add1 wheels =
        let carryUp (carry : bool) (wheel: Wheel) =
            ((if carry then inc wheel else wheel), (wheel = Pos9 && carry))
        wheels |> List.mapFold carryUp true |> fst

    let complement1 wheel = 
        withComplements |> List.find (fun w -> fst w = wheel) |> snd

    let complement wheels = 
        wheels |> List.map complement1 |> add1

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
