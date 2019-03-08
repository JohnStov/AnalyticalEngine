module Stack
    open Types

    let zero () = 
        { wheels = List.init STACKSIZE (fun _ -> Wheel.init()); negative = false }

    let create (value : Value) =
        let wheels = value.value |> Seq.map Wheel.fromChar |> Seq.toList;
        { 
            wheels = if value.negative then wheels |> Wheel.complement else wheels
            negative = value.negative }
    
    let carryUp carryIn (wheel, carryThrough) =
        let wheel' = if carryIn then Wheel.inc wheel else wheel
        let carryOut = (wheel = Pos9 && carryIn) || carryThrough 
        (wheel', carryOut)

    let add stack1 stack2 =
        let wheels, carry = 
            List.zip stack1.wheels stack2.wheels 
            |> List.map (fun (w1, w2) -> Wheel.add w1 w2)
            |> List.mapFold carryUp false
        { wheels = wheels; negative = stack1.negative <> stack2.negative <> carry }

    let fromString (s : string) =
        let negative, digits =
            match s.[0] with
            | '-' -> true, s.[1..]
            | '+' -> false, s.[1..]
            | _ -> false, s

        let wheels = digits |> Seq.toList |> List.map Wheel.fromChar |> List.rev
        if negative then
            {wheels = wheels |> Wheel.complement; negative = true}
        else
            {wheels = wheels; negative = false}

    let negate stack =
        { wheels = stack.wheels |> Wheel.complement; negative = not stack.negative}
