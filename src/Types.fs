module Types
    let ADDRESSDIGITS = 1
    let STORESIZE = pown 10 ADDRESSDIGITS
    let STACKSIZE = 10

    type Wheel = 
        | Pos0 
        | Pos1 
        | Pos2 
        | Pos3 
        | Pos4 
        | Pos5 
        | Pos6 
        | Pos7 
        | Pos8 
        | Pos9

    type Stack = { wheels: Wheel list; negative : bool }

    type Address = int

    type Operation = 
        | Add
        | Subtract 
        | Multiply
        | Divide

    type Variable = 
        | Load of Address
        | ZeroLoad of Address
        | Store of Address

    type Value = { negative: bool; value : string}
    
    type Number = {address: Address; value: Value}

    type Combinatorial = {forward: bool; conditional: bool; steps: int}

    type Action = 
        | Bell
        | Halt
        | Print
        | NoAction

    type Instruction =
        | Number of Number
        | Operation of Operation
        | Variable of Variable
        | Combinatorial of Combinatorial
        | Action of Action
        | Comment of string
        | NoOp

    type Store = Stack list

    type Mill = { ingress : Stack list; egress : Stack; axis : Stack; operation : Operation; ingressSelect: int; runUp: bool }

    type Engine = { store : Store; mill : Mill; instructionPointer : int }


