﻿// Learn more about F# at http://fsharp.org


open Fuchu
open FsUnit
open Types

[<Tests>]
let wheelTests = 
    testList "Wheel tests" [
        testCase "When a wheel at Pos0 is incremented the result should be Pos1" <| 
            fun _ -> Pos0 |> Wheel.inc |> should equal Pos1
        testCase "When a wheel at Pos9 is decremented the result should be Pos8" <| 
            fun _ -> Pos9 |> Wheel.dec |> should equal Pos8
        testCase "When a wheel at Pos9 is incremented the result should be Pos0" <| 
            fun _ -> Pos9 |> Wheel.dec |> should equal Pos8
        testCase "When a wheel at Pos0 is decremented the result should be Pos9" <| 
            fun _ -> Pos0 |> Wheel.inc |> should equal Pos1

        testCase "Negating 0 gives 0" <|
            fun _ -> {wheels = [Pos0; Pos0; Pos0; Pos0; Pos0]; negative = false} |> Stack.negate |> should equal {wheels = [Pos0; Pos0; Pos0; Pos0; Pos0]; negative = true}
        testCase "Negating 1 gives -1" <|
            fun _ -> {wheels = [Pos1; Pos0; Pos0; Pos0; Pos0]; negative = false} |> Stack.negate |> should equal {wheels = [Pos9; Pos9; Pos9; Pos9; Pos9]; negative = true}
        testCase "Negating -1 gives 1" <|
            fun _ -> {wheels = [Pos9; Pos9; Pos9; Pos9; Pos9]; negative = true} |> Stack.negate |> should equal {wheels = [Pos1; Pos0; Pos0; Pos0; Pos0]; negative = false}
        testCase "Negating 99999 gives -99999" <|
            fun _ -> {wheels = [Pos9; Pos9; Pos9; Pos9; Pos9]; negative = false} |> Stack.negate |> should equal {wheels = [Pos1; Pos0; Pos0; Pos0; Pos0]; negative = true}
        testCase "Negating -99999 gives 99999" <|
            fun _ -> {wheels = [Pos1; Pos0; Pos0; Pos0; Pos0]; negative = true} |> Stack.negate |> should equal {wheels = [Pos9; Pos9; Pos9; Pos9; Pos9]; negative = false}
        testCase "Negation is its inverse" <|
            fun _ -> {wheels = [Pos1; Pos2; Pos3; Pos4; Pos5]; negative = false} |> Stack.negate |> Stack.negate |> should equal {wheels = [Pos1; Pos2; Pos3; Pos4; Pos5]; negative = false}

        testCase "Adding 0 and 0 should be 0 with no carry" <|
            fun _ -> Wheel.add Pos0 Pos0 |> should equal (Pos0, false)
        testCase "Adding 1 and 0 should be 1 with no carry" <|
            fun _ -> Wheel.add Pos1 Pos0 |> should equal (Pos1, false)
        testCase "Adding 0 and 1 should be 1 with no carry" <|
            fun _ -> Wheel.add Pos0 Pos1 |> should equal (Pos1, false)
        testCase "Adding 1 and 1 should be 2 with no carry" <|
            fun _ -> Wheel.add Pos1 Pos1 |> should equal (Pos2, false)
        testCase "Adding 4 and 5 should be 9 with no carry" <|
            fun _ -> Wheel.add Pos4 Pos5 |> should equal (Pos9, false)
        testCase "Adding 5 and 5 should be 0 with carry" <|
            fun _ -> Wheel.add Pos5 Pos5 |> should equal (Pos0, true)
        testCase "Adding 6 and 5 should be 1 with carry" <|
            fun _ -> Wheel.add Pos6 Pos5 |> should equal (Pos1, true)
        testCase "Adding 9 and 9 should be 8 with carry" <|
            fun _ -> Wheel.add Pos9 Pos9 |> should equal (Pos8, true)

        testCase "Stack.fromString '0' is +0" <|
            fun _ -> Stack.fromString "0" |> should equal {wheels = [Pos0]; negative = false}
        testCase "Stack.fromString '+0' is +0" <|
            fun _ -> Stack.fromString "+0" |> should equal {wheels = [Pos0]; negative = false}
        testCase "Stack.fromString '-0' is -0" <|
            fun _ -> Stack.fromString "-0" |> should equal {wheels = [Pos0]; negative = true}
        testCase "Stack.fromString '123450' is +123450" <|
            fun _ -> Stack.fromString "123450" |> should equal {wheels = [Pos0;Pos5;Pos4;Pos3;Pos2;Pos1]; negative = false}
        testCase "Stack.fromString '+123450' is +123450" <|
            fun _ -> Stack.fromString "+123450" |> should equal {wheels = [Pos0;Pos5;Pos4;Pos3;Pos2;Pos1]; negative = false}
        testCase "Stack.fromString '-123450' is -876550" <|
            fun _ -> Stack.fromString "-123450" |> should equal {wheels = [Pos0; Pos5; Pos5; Pos6; Pos7; Pos8]; negative = true}

        testCase "Adding Stacks '0' and '0' should be '0' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "0"); (Stack.fromString "0")] 
                    |> should equal ({wheels = [Pos0]; negative = false}, false)
        testCase "Adding Stacks '1' and '1' should be '2' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "1"); (Stack.fromString "1")] 
                    |> should equal ({wheels = [Pos2]; negative = false}, false)
        testCase "Adding Stacks '00000' and '00000' should be '00000' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "00000"); (Stack.fromString "00000")] 
                    |> should equal ({wheels = [Pos0;Pos0;Pos0;Pos0;Pos0]; negative = false}, false)
        testCase "Adding Stacks '00001' and '00001' should be '00002' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "00001"); (Stack.fromString "00001")] 
                    |> should equal ({wheels = [Pos2;Pos0;Pos0;Pos0;Pos0]; negative = false}, false)
        testCase "Adding Stacks '00005' and '00005' should be '00010' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "00005"); (Stack.fromString "00005")] 
                    |> should equal ({wheels = [Pos0;Pos1;Pos0;Pos0;Pos0]; negative = false}, false)
        testCase "Adding Stacks '12345' and '12345' should be '14690' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "12345"); (Stack.fromString "12345")] 
                    |> should equal ({wheels = [Pos0;Pos9;Pos6;Pos4;Pos2]; negative = false}, false)
        testCase "Adding Stacks '99999' and '00001' should be '00000' with run up" <|
            fun _ -> Mill.add [(Stack.fromString "99999"); (Stack.fromString "00001")] 
                    |> should equal ({wheels = [Pos0;Pos0;Pos0;Pos0;Pos0]; negative = true}, true)
        testCase "Adding Stacks '99999' and '-00001' should be '99998' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "99999"); (Stack.fromString "-00001")] 
                    |> should equal ({wheels = [Pos8;Pos9;Pos9;Pos9;Pos9]; negative = false}, false)
        testCase "Adding Stacks '99999' and '-99999' should be '00000' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "99999"); (Stack.fromString "-99999")] 
                    |> should equal ({wheels = [Pos0;Pos0;Pos0;Pos0;Pos0]; negative = false}, false)
        testCase "Adding Stacks '0000000030' and '-0000000001' should be '0000000029' with no run up" <|
            fun _ -> Mill.add [(Stack.fromString "0000000030"); (Stack.fromString "-0000000001")] 
                    |> should equal ({wheels = [Pos9;Pos2;Pos0;Pos0;Pos0;Pos0;Pos0;Pos0;Pos0;Pos0]; negative = false}, false)
        testCase "Subtracting Stacks '0000000001' from '0000000030' should be '0000000029' with no run up" <|
            fun _ -> Mill.subtract [(Stack.fromString "0000000030"); (Stack.fromString "0000000001")] 
                    |> should equal ({wheels = [Pos9;Pos2;Pos0;Pos0;Pos0;Pos0;Pos0;Pos0;Pos0;Pos0]; negative = false}, false)
    ]

[<EntryPoint>]
let main argv =
    defaultMainThisAssembly argv
