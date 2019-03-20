module Stack
    open System
    open Types

    let zero () = 
        { wheels = List.init STACKSIZE (fun _ -> Wheel.init()); negative = false }

    let negate stack =
        let add1 wheels =
            let carryUp (carry : bool) (wheel: Wheel) =
                ((if carry then  wheel |> Wheel.inc else wheel), (wheel = Pos9 && carry))
            wheels |> List.mapFold carryUp true |> fst

        let complement wheels = 
            wheels |> List.map Wheel.complement |> add1

        { wheels = stack.wheels |> complement; negative = not stack.negative}

    let create (value : Value) =
        let result = { wheels = value.value |> Seq.map Wheel.fromChar |> Seq.toList |> List.rev; negative = false }
        if value.negative then 
            result |> negate 
        else 
            result
    
    let toString (stack : Stack) =
        let stack' = if stack.negative then (stack |> negate) else stack
        let str = stack'.wheels |> List.map Wheel.toChar |> List.rev |> Array.ofList |> String
        (if stack.negative then "-" else "+") + str
