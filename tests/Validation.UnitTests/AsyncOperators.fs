module Validation.UnitTests.AsyncOperators

open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote

open Validation.Operators.AsyncValidation

let (=!!) (lhs: 'value Task) (rhs: 'value) =
    lhs.Result =! rhs

[<Test>]
let ``map success`` () = ((+) 2) <!> Task.FromResult(Ok 1) =!! Ok 3

[<Test>]
let ``map failure`` () = ((+) 2) <!> Task.FromResult(Error ["test"]) =!! Error ["test"]

[<Test>]
let ``apply success`` () = Task.FromResult(Ok ((+) 2)) <*> Task.FromResult(Ok 1) =!! Ok 3

[<Test>]
let ``apply failure right`` () = Task.FromResult(Ok ((+) 2)) <*> Task.FromResult(Error ["test"]) =!! Error ["test"]

[<Test>]
let ``apply failure both`` () = Task.FromResult(Error ["test1"]) <*> Task.FromResult(Error ["test2"]) =!! Error ["test1"; "test2"]

[<Test>]
let ``bind success`` () = Task.FromResult(Ok 1) >>= (fun a -> Task.FromResult(Ok (a + 2))) =!! Ok 3

[<Test>]
let ``bind failure`` () = Task.FromResult(Ok 1) >>= (fun _ -> Task.FromResult(Error ["myError"])) =!! Error ["myError"]
