module Stack
    open Types

    let init () = 
        { wheels = Array.init STACKSIZE (fun _ -> Wheel.init()); negative = false }

    let create (value : Value) =
        let wheels = value.value |> Seq.map Wheel.fromChar |> Seq.toArray;
        let wheels = if value.negative then wheels |> Array.map Wheel.complement else wheels
        { wheels = value.value |> Seq.map Wheel.fromChar |> Seq.toArray; negative = value.negative }