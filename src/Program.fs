open Engine

[<EntryPoint>]
let main argv  =
    let engine = Engine.init ()
    let program = Reader.parseFile argv.[0]
    Engine.run program engine 
