module Validation.UnitTests.Validation

open NUnit.Framework
open Swensen.Unquote

[<Test>]
let ``map success`` () = Validation.map ((+) 2) (Ok 1) =! Ok 3

[<Test>]
let ``map failure`` () = Validation.map ((+) 2) (Error ["test"]) =! Error ["test"]

[<Test>]
let ``mapError success`` () = Validation.mapError (List.map ((+) 2)) (Ok 1) =! Ok 1

[<Test>]
let ``mapError failure`` () = Validation.mapError (List.map ((+) 2)) (Error [1; 2]) =! Error [3; 4]

[<Test>]
let ``apply success`` () = Validation.apply (Ok ((+) 2)) (Ok 1) =! Ok 3

[<Test>]
let ``apply failure right`` () = Validation.apply (Ok ((+) 2)) (Error ["test"]) =! Error ["test"]

[<Test>]
let ``apply failure both`` () = Validation.apply (Error ["test1"]) (Error ["test2"]) =! Error ["test1"; "test2"]

[<Test>]
let ``bind success`` () = Validation.bind (fun a -> Ok (a + 2)) (Ok 1) =! Ok 3

[<Test>]
let ``bind failure`` () = Validation.bind (fun _ -> Error ["myError"]) (Ok 1) =! Error ["myError"]
