module Types
    let STORESIZE = 1000
    let STACKSIZE = 50

    type Wheel = | Pos0 | Pos1 | Pos2 | Pos3 | Pos4 | Pos5 | Pos6 | Pos7 | Pos8 | Pos9

    type Stack = { wheels: Wheel list; negative : bool }

    type Address = int

    type Op = 
        | Add
        | Subtract 
        | Multiply
        | Divide

    type Var = 
        | Load of Address
        | ZeroLoad of Address
        | Store of Address

    type Value = { negative: bool; value : string}
    
    type Num = {address: Address; value: Value}

    type Instruction =
        | Number of Num
        | Operation of Op
        | Variable of Var
        | Comment of string
        | NoOp

    type Store = Stack list

    type Mill = { ingress : Stack list; egress : Stack; operation : Op; ingressSelect: int }

    type Engine = { store : Store; mill : Mill }


