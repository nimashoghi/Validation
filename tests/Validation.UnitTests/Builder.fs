module Validation.UnitTests.Builder

open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote

open Validation.Builder

let (=!!) (lhs: 'value ValueTask) (rhs: 'value) =
    lhs.Result =! rhs

[<Test>]
let ``basic`` () =
    validation {
        validate v in Ok 1
        return v
    } =!! Ok 1

[<Test>]
let ``nested success`` () =
    let f () =
        validation {
            validate v in Ok 1
            return v
        }

    validation {
        validate v in f ()
        return v + 5
    } =!! Ok 6

[<Test>]
let ``nested success multiple`` () =
    let f () =
        validation {
            validate v in Ok 1
            return v
        }
    let g () =
        validation {
            validate v in Ok "hello world"
            return v
        }

    validation {
        validate v in f ()
        validate w in g ()
        return
            {|
                v = v
                w = w
            |}
    } =!! Ok {|v = 1; w = "hello world"|}


[<Test>]
let ``nested failure`` () =
    let f () =
        validation {
            validate v in Error ["hello"]
            return v
        }

    validation {
        validate v in f ()
        return v + 5
    } =!! Error ["hello"]

[<Test>]
let ``nested failure multiple`` () =
    let f () =
        validation {
            validate v in Error ["hello"]
            return v
        }
    let g () =
        validation {
            validate v in Error ["world"]
            return v
        }

    validation {
        validate v in f ()
        validate w in g ()
        return
            {|
                v = v
                w = w
            |}
    } =!! Error ["hello"; "world"]

[<Test>]
let ``returnFrom nested success multiple`` () =
    let f () =
        validation {
            validate v in Ok 1
            return v
        }
    let g () =
        validation {
            validate v in Ok "hello world"
            return v
        }
    let k () =
        validation {
            validate v in f ()
            validate w in g ()
            return
                {|
                    v = v
                    w = w
                |}
        }

    validation {
        return! k ()
    } =!! Ok {|v = 1; w = "hello world"|}

[<Test>]
let ``returnFrom nested failure multiple`` () =
    let f () =
        validation {
            validate v in Error ["hello"]
            return v
        }
    let g () =
        validation {
            validate v in Error ["world"]
            return v
        }
    let k () =
        validation {
            validate v in f ()
            validate w in g ()
            return
                {|
                    v = v
                    w = w
                |}
        }

    validation {
        return! k ()
    } =!! Error ["hello"; "world"]

[<Test>]
let ``Zero`` () =
    validation {
        ()
    } =!! Error []

[<Test>]
let ``ValueTask support`` () =
    validation {
        validate a in ValueTask<_> (Ok 1)
        return a
    } =!! Ok 1

// [<Test>]
// let ``yield`` () =
//     validation {
//         yield "Hello world"
//     } =!! Error ["Hello world"]

// [<Test>]
// let ``yieldFrom`` () =
//     validation {
//         yield! ["Hello world"]
//     } =!! Error ["Hello world"]

// [<Test>]
// let ``yieldFrom Result`` () =
//     validation {
//         yield! Error ["Hello world"]
//     } =!! Error ["Hello world"]
