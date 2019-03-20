module Mill
    open Types

    let init () =
        {
            ingress = [Stack.zero (); Stack.zero ()]
            egress = Stack.zero ()
            axis = Stack.zero () 
            operation = Add
            ingressSelect = 0 
            runUp = false
        }

    let setOp op mill =
        { mill with operation = op }

    let add (ingress : Stack list) =
        let carryUp carryIn (wheel, carryThrough) =
            let wheel' = if carryIn then Wheel.inc wheel else wheel
            let carryOut = (wheel = Pos9 && carryIn) || carryThrough 
            (wheel', carryOut)

        let wheels, carry = 
            List.zip ingress.[0].wheels ingress.[1].wheels 
            |> List.map (fun (w1, w2) -> Wheel.add w1 w2)
            |> List.mapFold carryUp false
        let negative = ingress.[0].negative <> ingress.[1].negative <> carry
        let runUp = ingress.[0].negative <> negative
        ({ wheels = wheels; negative = negative }, runUp )

    let subtract (ingress : Stack list) =
        let result = add [ingress.[0]; (ingress.[1] |> Stack.negate)]
        result

    let multiply ingress =
        (Stack.zero (), false)

    let divide ingress =
        (Stack.zero (), true)

    let performOp (mill : Mill) =
        let opFn = 
            match mill.operation with
            | Add ->add
            | Subtract -> subtract
            | Multiply -> multiply
            | Divide -> divide
        let result, runUp = opFn mill.ingress
        { mill with egress = result; axis = result; runUp = runUp }

    let load stack (mill : Mill) =
        let ingress' = if mill.ingressSelect = 0 then [ stack; mill.ingress.[1] ] else [ mill.ingress.[0] ; stack ]
        let mill' = { mill with ingress = ingress'; axis = stack; ingressSelect = (mill.ingressSelect + 1) % 2 }
        if mill.ingressSelect = 1 then 
            performOp mill'
        else
            mill'
            
