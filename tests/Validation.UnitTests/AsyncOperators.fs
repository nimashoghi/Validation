module Validation.UnitTests.AsyncOperators

open System.Threading.Tasks
open NUnit.Framework
open FSharp.Utils.Tasks
open Swensen.Unquote

open Validation.Operators.AsyncValidation

let (=!!) (lhs: 'value ValueTask) (rhs: 'value) =
    lhs.Result =! rhs

[<Test>]
let ``map success`` () = ((+) 2) <!> vtask { return (Ok 1) } =!! Ok 3

[<Test>]
let ``map failure`` () = ((+) 2) <!> vtask { return (Error ["test"]) } =!! Error ["test"]

[<Test>]
let ``apply success`` () = vtask { return (Ok ((+) 2)) } <*> vtask { return (Ok 1) } =!! Ok 3

[<Test>]
let ``apply failure right`` () = vtask { return (Ok ((+) 2)) } <*> vtask { return (Error ["test"]) } =!! Error ["test"]

[<Test>]
let ``apply failure both`` () = vtask { return (Error ["test1"]) } <*> vtask { return (Error ["test2"]) } =!! Error ["test1"; "test2"]

[<Test>]
let ``bind success`` () = vtask { return (Ok 1) } >>= (fun a -> vtask { return (Ok (a + 2)) }) =!! Ok 3

[<Test>]
let ``bind failure`` () = vtask { return (Ok 1) } >>= (fun _ -> vtask { return (Error ["myError"]) }) =!! Error ["myError"]
