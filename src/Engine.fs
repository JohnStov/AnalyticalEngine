module Engine
    open Types
    open System

    let branch combinatorial engine =
        let newIp =
            match combinatorial.forward, combinatorial.conditional, engine.mill.runUp, combinatorial.steps with
            | true,  false,  _,    s
            | true,  true,  true,  s -> engine.instructionPointer + s
            | false, false, _,     s
            | false, true,  true,  s -> engine.instructionPointer - s
            | _,     true,  false, _ -> engine.instructionPointer
        { engine with instructionPointer = newIp }

    let act action engine =
        match action with
        | Bell -> 
            printfn "Ding!"
            engine
        | Halt -> 
            { engine with instructionPointer = -1 }
        | Print -> 
            printfn "%s" (engine.mill.axis |> Stack.toString)
            engine
        | _ -> engine
    
    let move var engine =
        match var with 
        | Load address -> 
            { engine with mill = Mill.load (Store.read address engine.store) engine.mill }
        | ZeroLoad address ->
            { 
                engine with 
                    mill = Mill.load (Store.read address engine.store) engine.mill
                    store = engine.store.[0..address-1] @ [Stack.zero ()] @ engine.store.[address+1..]
            }
        | Store address -> 
            { engine with store = engine.store.[0..address-1] @ [engine.mill.egress] @ engine.store.[address+1..] }

    let step instruction engine =
        match instruction with
        | Number num -> { engine with store = Store.set num engine.store }
        | Operation op -> { engine with mill = Mill.setOp op engine.mill }
        | Variable var ->  move var engine
        | Combinatorial c -> branch c engine
        | Action a -> act a engine
        | Comment c -> engine
        | NoOp -> engine

    let dump engine =
        printfn "Instruction Pointer: %d" engine.instructionPointer
        printfn "Store:"
        engine.store |> List.mapi (fun i stack -> printfn "\t %d: %s" i (stack |> Stack.toString)) |> ignore
        printfn "Mill:"
        printfn "\t Ingress:"
        engine.mill.ingress |> List.mapi (fun i stack -> printfn "\t\t %d: %s" i (stack |> Stack.toString)) |> ignore
        printfn "\t Egress:"
        printfn "\t\t    %s" (engine.mill.egress |> Stack.toString)
        printfn "\t Axis:"
        printfn "\t\t    %s" (engine.mill.axis |> Stack.toString)
        printfn "\t Operation: %A" engine.mill.operation
        printfn "\t IngressSelect: %A" engine.mill.ingressSelect
        printfn "\t RunUp: %A" engine.mill.runUp

    let execute (program : Instruction array) engine = 
        let rec executeRec engine =
 //           dump engine
            if engine.instructionPointer < 0 then
                engine
            else
                let engine' = step program.[engine.instructionPointer] engine
                let ip = (if engine'.instructionPointer < 0 then engine'.instructionPointer else engine'.instructionPointer+1)
//                Console.ReadLine () |> ignore
                executeRec { engine' with instructionPointer = ip }
    
        executeRec engine

    let run program engine =
        execute program engine |> ignore
        0

    let init ()=
        { store = Store.init (); mill = Mill.init (); instructionPointer = 0 }


