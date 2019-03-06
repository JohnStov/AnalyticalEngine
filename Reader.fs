module Reader
    open System
    open System.IO
    open System.Text.RegularExpressions

    open Types

    let numberRegex = Regex("^N(\d{1,3})\s([+-])(?\d+)\s.*$")
    let operationRegex = Regex("^([+-*/])$")
    let variableRegex = Regex("^([LZS])(\d{1,3})\s.*$")
    let commentRegex = Regex("^\s(.*)$")

    let (|Match|_|) (regex : Regex) str =
        let m = regex.Match(str)
        if m.Success then 
            Some [ for i in 1 .. (m.Groups.Count-1) do yield m.Groups.[i].Value ]
        else
            None

    let parseLine str =
        match str with
        | Match numberRegex groups -> 
            Number { 
                address = groups.[0] |> int 
                value = { 
                    negative = (groups.[1] = "-") 
                    value = (String.init (STACKSIZE - groups.[2].Length) (fun _ -> "0")) + groups.[2] }}
        | Match operationRegex groups -> 
            Operation (match groups.[0] with 
                      | "-" -> Subtract 
                      | "*" -> Multiply 
                      | "/" -> Divide
                      | "+" | _ -> Add)
        | Match variableRegex groups -> 
            Variable (match groups.[0] with 
                     | "Z" -> ZeroLoad (groups.[1] |> int) 
                     | "S" -> Store (groups.[1] |> int)
                     | "L" | _ -> Load (groups.[1] |> int))
        | Match commentRegex groups -> 
            Comment groups.[0]
        | _ -> 
            NoOp

    let parseString (str : string) =
        let lines = str.Split(Environment.NewLine) |> Array.toList
        lines |> List.map parseLine

    let parseFile path =
        let lines = File.ReadLines(path) |> Seq.toList
        lines |> List.map parseLine

