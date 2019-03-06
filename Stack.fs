module Stack
    open Types

    let zero () = 
        { wheels = List.init STACKSIZE (fun _ -> Wheel.init()); negative = false }

    let create (value : Value) =
        let wheels = value.value |> Seq.map Wheel.fromChar |> Seq.toList;
        { 
            wheels = if value.negative then wheels |> Wheel.complement else wheels
            negative = value.negative }