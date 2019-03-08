module Mill
    open Types

    let init () =
        {
            ingress = [Stack.zero (); Stack.zero ()]
            egress = Stack.zero ()
            operation = Add
            ingressSelect = 0 
            runUp = false
        }

    let setOp mill op =
        { mill with operation = op }

    let add (ingress : Stack list) =
        Stack.add ingress.[0] ingress.[1]

    let subtract (ingress : Stack list) =
        Stack.add ingress.[0] (ingress.[1] |> Stack.negate)

    let multiply ingress =
        Stack.zero ()

    let divide ingress =
        Stack.zero ()

    let performOp (mill : Mill) =
        match mill.operation with
        | Add -> { mill with egress = add mill.ingress }
        | Subtract -> { mill with egress = subtract mill.ingress }
        | Multiply -> { mill with egress = multiply mill.ingress }
        | Divide -> { mill with egress = divide mill.ingress }
    
    let load (mill : Mill) stack =
        let ingress' = if mill.ingressSelect = 0 then [ stack; mill.ingress.[1] ] else [ mill.ingress.[0] ; stack ]
        let mill' = { 
            mill with 
                ingress = ingress'
                ingressSelect = (mill.ingressSelect + 1) % 2 
        }
        if mill'.ingressSelect = 1 then performOp mill' else mill'
            
