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

[<Test>]
let ``toOption success`` () = Validation.toOption (Ok 1) =! Some 1

[<Test>]
let ``toOption failure`` () = Validation.toOption (Error ["myError"]) =! None

[<Test>]
let ``toValueOption success`` () = Validation.toValueOption (Ok 1) =! ValueSome 1

[<Test>]
let ``toValueOption failure`` () = Validation.toValueOption (Error ["myError"]) =! ValueNone

[<Test>]
let ``ofOptionWith success`` () = Validation.ofOptionWith (fun () -> ["myError"]) (Some 1) =! Ok 1

[<Test>]
let ``ofOptionWith failure`` () = Validation.ofOptionWith (fun () -> ["myError"]) None =! Error ["myError"]

[<Test>]
let ``ofValueOptionWith success`` () = Validation.ofValueOptionWith (fun () -> ["myError"]) (ValueSome 1) =! Ok 1

[<Test>]
let ``ofValueOptionWith failure`` () = Validation.ofValueOptionWith (fun () -> ["myError"]) ValueNone =! Error ["myError"]

[<Test>]
let ``ofOption success`` () = Validation.ofOption ["myError"] (Some 1) =! Ok 1

[<Test>]
let ``ofOption failure`` () = Validation.ofOption ["myError"] None =! Error ["myError"]

[<Test>]
let ``ofValueOption success`` () = Validation.ofValueOption ["myError"] (ValueSome 1) =! Ok 1

[<Test>]
let ``ofValueOption failure`` () = Validation.ofValueOption ["myError"] ValueNone =! Error ["myError"]

[<Test>]
let ``tryPick ok success`` () =
    Validation.tryPick
        ["myError"]
        (fun x -> if x = 1 then Some "hello" else None)
        (Ok 1)
    =! Ok "hello"

[<Test>]
let ``tryPick ok failure`` () =
    Validation.tryPick
        ["myError"]
        (fun x -> if x = 1 then Some "hello" else None)
        (Ok 2)
    =! Error ["myError"]

[<Test>]
let ``tryPick error success`` () =
    Validation.tryPick
        ["myError"]
        (fun x -> if x = 1 then Some "hello" else None)
        (Error ["myOtherError"])
    =! Error ["myOtherError"]

[<Test>]
let ``tryPick error failure`` () =
    Validation.tryPick
        ["myError"]
        (fun x -> if x = 1 then Some "hello" else None)
        (Error ["myOtherError"])
    =! Error ["myOtherError"]


[<Test>]
let ``tryFind ok success`` () =
    Validation.tryFind
        ["myError"]
        (fun x -> x = 1)
        (Ok 1)
    =! Ok 1

[<Test>]
let ``tryFind ok failure`` () =
    Validation.tryFind
        ["myError"]
        (fun x -> x = 1)
        (Ok 2)
    =! Error ["myError"]

[<Test>]
let ``tryFind error success`` () =
    Validation.tryFind
        ["myError"]
        (fun x -> x = 1)
        (Error ["myOtherError"])
    =! Error ["myOtherError"]

[<Test>]
let ``tryFind error failure`` () =
    Validation.tryFind
        ["myError"]
        (fun x -> x = 1)
        (Error ["myOtherError"])
    =! Error ["myOtherError"]

[<Test>]
let ``contains success`` () = Validation.contains 1 (Ok 1) =! true

[<Test>]
let ``contains failure`` () = Validation.contains 1 (Error ["myError"]) =! false

[<Test>]
let ``defaultValue default ok underlying ok`` () = Validation.defaultValue (Ok 1) (Ok 2) =! Ok 2

[<Test>]
let ``defaultValue default ok underlying empty`` () = Validation.defaultValue (Ok 1) (Error []) =! Ok 1

[<Test>]
let ``defaultValue default ok underlying error`` () = Validation.defaultValue (Ok 1) (Error ["myError"]) =! Error ["myError"]

[<Test>]
let ``defaultValue default error underlying ok`` () = Validation.defaultValue (Error ["myError2"]) (Ok 2) =! Ok 2

[<Test>]
let ``defaultValue default error underlying empty`` () = Validation.defaultValue (Error ["myError2"]) (Error []) =! Error ["myError2"]

[<Test>]
let ``defaultValue default error underlying error`` () = Validation.defaultValue (Error ["myError2"]) (Error ["myError"]) =! Error ["myError"]

[<Test>]
let ``defaultWith default ok underlying ok`` () = Validation.defaultWith (fun () -> Ok 1) (Ok 2) =! Ok 2

[<Test>]
let ``defaultWith default ok underlying empty`` () = Validation.defaultWith (fun () -> Ok 1) (Error []) =! Ok 1

[<Test>]
let ``defaultWith default ok underlying error`` () = Validation.defaultWith (fun () -> Ok 1) (Error ["myError"]) =! Error ["myError"]

[<Test>]
let ``defaultWith default error underlying ok`` () = Validation.defaultWith (fun () -> Error ["myError2"]) (Ok 2) =! Ok 2

[<Test>]
let ``defaultWith default error underlying empty`` () = Validation.defaultWith (fun () -> Error ["myError2"]) (Error []) =! Error ["myError2"]

[<Test>]
let ``defaultWith default error underlying error`` () = Validation.defaultWith (fun () -> Error ["myError2"]) (Error ["myError"]) =! Error ["myError"]

[<Test>]
let ``defaultSuccess underlying ok`` () = Validation.defaultSuccess 1 (Ok 2) =! Ok 2

[<Test>]
let ``defaultSuccess underlying empty`` () = Validation.defaultSuccess 1 (Error []) =! Ok 1

[<Test>]
let ``defaultSuccess underlying error`` () = Validation.defaultSuccess 1 (Error ["myError"]) =! Error ["myError"]

[<Test>]
let ``defaultError underlying ok`` () = Validation.defaultError ["myError2"] (Ok 2) =! Ok 2

[<Test>]
let ``defaultError underlying empty`` () = Validation.defaultError ["myError2"] (Error []) =! Error ["myError2"]

[<Test>]
let ``defaultError underlying error`` () = Validation.defaultError ["myError2"] (Error ["myError"]) =! Error ["myError"]

[<Test>]
let ``defaultSuccessWith underlying ok`` () = Validation.defaultSuccessWith (fun () -> 1) (Ok 2) =! Ok 2

[<Test>]
let ``defaultSuccessWith underlying empty`` () = Validation.defaultSuccessWith (fun () -> 1) (Error []) =! Ok 1

[<Test>]
let ``defaultSuccessWith underlying error`` () = Validation.defaultSuccessWith (fun () -> 1) (Error ["myError"]) =! Error ["myError"]

[<Test>]
let ``defaultErrorWith underlying ok`` () = Validation.defaultErrorWith (fun () -> ["myError2"]) (Ok 2) =! Ok 2

[<Test>]
let ``defaultErrorWith underlying empty`` () = Validation.defaultErrorWith (fun () -> ["myError2"]) (Error []) =! Error ["myError2"]

[<Test>]
let ``defaultErrorWith underlying error`` () = Validation.defaultErrorWith (fun () -> ["myError2"]) (Error ["myError"]) =! Error ["myError"]
