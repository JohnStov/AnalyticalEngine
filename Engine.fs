module Engine
    open Types

    let move engine var =
        match var with 
        | Load address -> { engine with mill = Mill.load engine.mill (Store.read engine.store address) }
        | ZeroLoad address ->
            { 
                engine with 
                    mill = Mill.load engine.mill (Store.read engine.store address)
                    store = engine.store.[0..address-1] @ [Stack.zero ()] @ engine.store.[address+1..]
            }
        | Store address -> { engine with mill = Mill.load engine.mill (Store.read engine.store address) }

    let execute engine instruction =
        match instruction with
        | Number num -> { engine with store = Store.set engine.store num }
        | Operation op -> { engine with mill = Mill.setOp engine.mill op }
        | Variable var -> move engine var
        | Comment c -> engine
        | NoOp -> engine

    let run engine instructions =
        instructions |> List.fold execute engine |> ignore
        0

    let init ()=
        { store = Store.init (); mill = Mill.init () }


