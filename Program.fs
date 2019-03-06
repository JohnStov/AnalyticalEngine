open Engine

[<EntryPoint>]
let main argv =
    let engine = Engine.init STORESIZE STACKSIZE
    
    let fname = argv.[1]
    let program = Reader.parseFile fname
    Engine.run engine program
