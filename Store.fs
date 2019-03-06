module Store
    open Types

    let init () =
        Array.init STORESIZE (fun _ -> Stack.init ())
    
    let set (store : Store) num =
        let index = num.address
        store.[index] <- Stack.create num.value 

    let read store address =
        