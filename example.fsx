let x = 1

let add a b =
    a + b

let add1 = add 1

let addLots (fn : (int -> int)) y x =
    (fn y) + x 

let y = add1 2

let si = ("Hello", 2)

type MyRecord = {name: string; value: int}

let r = {name = "Hello"; value= 2}
let r2 = {r with name = "World"}

type MathOp =
        | Add of string
        | Subtract of int
        | Multiply of string * string
        | Divide

let fn (a: MathOp) =
    match a with
    | Add s -> printfn "%s" s
    | Multiply (s1, s2) -> printfn "%s,%s" s1 s2
    | _ ->
