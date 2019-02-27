module Validation

type Validation<'value, 'error> = Result<'value, 'error list>

/// **Description**
///     Applies the function `f` to the value inside container `x`.
let map (f: _ -> _) (x: Validation<_, _>): Validation<_, _> = Result.map f x

/// **Description**
///     Applies the function `f` to the error inside container `x`.
let mapError (f: _ -> _) (x: Validation<_, _>): Validation<_, _> = Result.mapError f x

/// **Description**
///     Applies the function `f` to the error inside container `x` and flattens the result.
let bind (f: _ -> Validation<_, _>) (x: Validation<_, _>): Validation<_, _> = Result.bind f x

/// **Description**
///     Applies the function inside the container `f` to the value inside the container `x`.
let apply (f: Validation<_ -> _, _>) (x: Validation<_, _>): Validation<_, _> =
    match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error lhs, Error rhs -> Error (lhs @ rhs)
    | Error lhs, _ -> Error lhs
    | _, Error rhs -> Error rhs

module Operators =
    /// **Description**
    ///     Validation map operator
    let (<!>) f x = map f x

    /// **Description**
    ///     Validation apply operator
    let (<*>) f x = apply f x

    /// **Description**
    ///     Validation bind operator
    let (>>=) x f = bind f x

module Builder =
    open Operators

    type ValidationBuilder<'error> () =
        member __.Yield (_: unit): Validation<_, 'error> = Ok (unbox<_> null)
        member __.For (x: Validation<'value, 'error>, f: 'value -> Validation<_, 'error>) = bind f x
        [<CustomOperation ("validate", IsLikeZip = true)>]
        member __.Validate (outer: Validation<'outer, 'error>, inner: Validation<'inner, 'error>, f: 'outer -> 'inner -> 'result): Validation<'result, 'error> =
            f <!> outer <*> inner
        member __.Return (x: 'value): Validation<'value, 'error> = Ok x
        member __.ReturnFrom (x:  Validation<_, 'error>) = x

    let validation<'error> = ValidationBuilder<'error> ()
