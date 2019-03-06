module Engine
    open Types

    let move engine var =
        match var with 
        | Load address -> { engine with mill = Mill.load (Store.read address) }
        | ZeroLoad address ->
            let value = Store.zeroRead Address
            { engine with mill = Mill.load value }
        | Store address -> { engine with mill = Mill.load (Store.read address) }

    let execute engine instruction =
        match instruction with
        | Number num -> { engine with store = Store.set engine.store num }
        | Operation op -> { engine with mill = Mill.setOp engine.mill op }
        | Variable var -> move engine var
        | Comment c -> engine
        | NoOp -> engine

    let run engine instructions =
        instructions |> List.map (execute engine)

    let init ()=
        { store = Store.init (); mill = Mill.init () }


