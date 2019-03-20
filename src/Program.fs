open Engine

[<EntryPoint>]
let main argv  =
    let engine = Engine.init ()
    let program = Reader.parseFile "Fibonacci.ana"
    Engine.run program engine 
