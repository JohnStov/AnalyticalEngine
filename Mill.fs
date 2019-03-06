module Mill
    open Types

    let init () =
        {
            ingress = [Stack.zero (); Stack.zero ()]
            egress = Stack.zero ()
            operation = Add
            ingressSelect = 0 
        }

    let setOp mill op =
        { mill with operation = op }
    
    let load (mill : Mill) stack =
        { 
            mill with 
                ingress = if mill.ingressSelect = 0 then [ stack; mill.ingress.[1] ] else [ mill.ingress.[0] ; stack ]
                ingressSelect = (mill.ingressSelect + 1) % 2 
        }
            
