module Validation.UnitTests.SyncOperators

open NUnit.Framework
open Swensen.Unquote

open Validation.Operators.Validation

[<Test>]
let ``map success`` () = ((+) 2) <!> Ok 1 =! Ok 3

[<Test>]
let ``map failure`` () = ((+) 2) <!> Error ["test"] =! Error ["test"]

[<Test>]
let ``apply success`` () = Ok ((+) 2) <*> Ok 1 =! Ok 3

[<Test>]
let ``apply failure right`` () = Ok ((+) 2) <*> Error ["test"] =! Error ["test"]

[<Test>]
let ``apply failure both`` () = Error ["test1"] <*> Error ["test2"] =! Error ["test1"; "test2"]

[<Test>]
let ``bind success`` () = Ok 1 >>= (fun a -> Ok (a + 2)) =! Ok 3

[<Test>]
let ``bind failure`` () = Ok 1 >>= (fun _ -> Error ["myError"]) =! Error ["myError"]
