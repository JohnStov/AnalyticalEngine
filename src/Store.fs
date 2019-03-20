module Store
    open Types

    let init () =
        List.init STORESIZE (fun _ -> Stack.zero ())
    
    let set num (store : Store) =
        store.[0 .. num.address-1] @ [ Stack.create num.value ] @ store.[num.address+1 ..]

    let read address (store : Store) =
        store.[address]

