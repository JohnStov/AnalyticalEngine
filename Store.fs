module Store
    open Types

    let init () =
        List.init STORESIZE (fun _ -> Stack.zero ())
    
    let set (store : Store) num =
        store.[0 .. num.address-1] @ [ Stack.create num.value ] @ store.[num.address+1 ..]

    let read (store : Store) address =
        store.[address]

