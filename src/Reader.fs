module Reader
    open System
    open System.IO
    open System.Text.RegularExpressions

    open Types

    let (|Match|_|) regexStr str =
        let regex = Regex regexStr
        let m = regex.Match(str)
        if m.Success then 
            Some [ for i in 1 .. (m.Groups.Count-1) do yield m.Groups.[i].Value ]
        else
            None

    let zeroPadLeft length (str : string) = 
        (String.init (length - str.Length) (fun _ -> "0")) + str
    
    let parseLine str =
        match str with
        | Match ("^N(\d{1,3})\s+([+-]?)(\d+)\s?.*$") groups -> 
            Number { 
                address = groups.[0] |> int 
                value = { 
                    negative = (groups.[1] = "-") 
                    value = zeroPadLeft STACKSIZE groups.[2] }}
        | Match "^([\+\-\*\/]\s?.*)$" groups -> 
            Operation (match groups.[0] with 
                      | "-" -> Subtract 
                      | "*" -> Multiply 
                      | "/" -> Divide
                      | "+" | _ -> Add)
        | Match ("^C([FB])([\+\?])(\d+)\s?.*$") groups -> 
            Combinatorial { forward = groups.[0] = "F"; conditional = groups.[1] = "?"; steps = groups.[2] |> int }
        | Match ("^([HPB])\s?.*$") groups -> 
            Action (match groups.[0] with
                    | "H" -> Halt
                    | "P" -> Print
                    | "B" -> Bell
                    | _ -> NoAction)
        | Match ("^([LZS])(\d{1,3})\s?.*$") groups -> 
            Variable (match groups.[0] with 
                     | "Z" -> ZeroLoad (groups.[1] |> int) 
                     | "S" -> Store (groups.[1] |> int)
                     | "L" | _ -> Load (groups.[1] |> int))
        | Match "^\s(.*)$" groups -> 
            Comment groups.[0]
        | _ -> 
            NoOp

    let parseString (str : string) =
        let lines = str.Split(Environment.NewLine) |> Array.toList
        lines |> List.map parseLine |> List.toArray

    let parseFile path =
        let lines = File.ReadLines(path) |> Seq.toList
        lines |> List.map parseLine |> List.toArray

