module Mill
    open Types

    let init stackSize =
        {
            ingress = [|Stack.init stackSize; Stack.init stackSize|]
            egress = Stack.init stackSize
            operation = Add
            ingressSelect = 0 }

    let setOp mill op =
        { mill with operation = op }