namespace global

type Validation<'value, 'error> = Result<'value, 'error list>

module Validation =
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

    /// **Description**
    ///     Converts `x` to an option.
    let toOption (x: Validation<'value, 'error>): 'value option =
        match x with
        | Ok value -> Some value
        | _ -> None

    /// **Description**
    ///     Converts `x` to a value option.
    let toValueOption (x: Validation<'value, 'error>): 'value voption =
        match x with
        | Ok value -> ValueSome value
        | _ -> ValueNone

    /// **Description**
    ///     Converts `x` from a `'value option` to a `Validation<'value, 'error>`, returning the result of `error ()` if `x` is `None`.
    let ofOptionWith (error: unit -> 'error list) (x: 'value option): Validation<'value, 'error> =
        match x with
        | Some value -> Ok value
        | None -> Error (error ())

    /// **Description**
    ///     Converts `x` from a `'value option` to a `Validation<'value, 'error>`, returning `error` if `x` is `None`.
    let ofOption (error: 'error list) (x: 'value option): Validation<'value, 'error> =
        ofOptionWith (fun () -> error) x

    /// **Description**
    ///     Converts `x` from a `'value voption` to a `Validation<'value, 'error>`, returning the result of `error ()` if `x` is `ValueNone`.
    let ofValueOptionWith (error: unit -> 'error list) (x: 'value voption): Validation<'value, 'error> =
        match x with
        | ValueSome value -> Ok value
        | ValueNone -> Error (error ())

    /// **Description**
    ///     Converts `x` from a `'value voption` to a `Validation<'value, 'error>`, returning `error` if `x` is `ValueNone`.
    let ofValueOption (error: 'error list) (x: 'value voption): Validation<'value, 'error> =
        ofValueOptionWith (fun () -> error) x

    /// **Description**
    ///     Calls `f` on the value inside of `x`. If the result is `Some`, it returns that value. Otherwise, returns the result of `error ()`.
    let tryPickWith (error: unit -> 'error list) (f: 'value -> 'result option) (x: Validation<'value, 'error>): Validation<'result, 'error> =
        bind (f >> ofOptionWith error) x

    /// **Description**
    ///     Calls `f` on the value inside of `x`. If the result is `Some`, it returns that value. Otherwise, returns `error`.
    let tryPick (error: 'error list) (f: 'value -> 'result option) (x: Validation<'value, 'error>): Validation<'result, 'error> =
        tryPickWith (fun () -> error) f x

    /// **Description**
    ///     Calls `f` on the value inside of `x`. If the result is `true`, it returns that value. Otherwise, returns `error`.
    let tryFind (error: 'error list) (f: 'value -> bool) (x: Validation<'value, 'error>): Validation<'value, 'error> =
        tryPickWith (fun () -> error) (fun value -> if f value then Some value else None) x

    /// **Description**
    ///     Returns `true` if the value inside of `x` is equal to `value`.
    let contains (value: 'value) (x: Validation<'value, 'error>): bool =
        match x with
        | Ok value' when value' = value -> true
        | _ -> false

    /// **Description**
    ///     Flattens the nested validation object `x` of type `Validation<Validation<'value, 'error>>` into a `Validation<'value, 'error>`
    let flatten (x: Validation<Validation<'value, 'error>, 'error>): Validation<'value, 'error> = bind id x

    /// **Description**
    ///     If `x` is `Error []`, returns the result of calling `f`. Otherwise, it forwards `x`.
    let defaultWith (f: unit -> Validation<'value, 'error>) (x: Validation<'value, 'error>): Validation<'value, 'error> =
        match x with
        | Ok value -> Ok value
        | Error [] -> f ()
        | Error err -> Error err

    /// **Description**
    ///     If `x` is `Error []`, returns `defaultValue`. Otherwise, it forwards `x`.
    let defaultValue (defaultValue: Validation<'value, 'error>) (x: Validation<'value, 'error>): Validation<'value, 'error> =
        defaultWith (fun () -> defaultValue) x

    /// **Description**
    ///     If `x` is `Error []`, returns the result of calling `f >> Ok`. Otherwise, it forwards `x`.
    let defaultSuccessWith (f: unit -> 'value) (x: Validation<'value, 'error>): Validation<'value, 'error> =
        defaultWith (f >> Ok) x

    /// **Description**
    ///     If `x` is `Error []`, returns `Ok defaultValue`. Otherwise, it forwards `x`.
    let defaultSuccess (defaultValue: 'value) (x: Validation<'value, 'error>): Validation<'value, 'error> =
        defaultSuccessWith (fun () -> defaultValue) x

    /// **Description**
    ///     If `x` is `Error []`, returns the result of calling `f >> Error`. Otherwise, it forwards `x`.
    let defaultErrorWith (f: unit -> 'error list) (x: Validation<'value, 'error>): Validation<'value, 'error> =
        defaultWith (f >> Error) x

    /// **Description**
    ///     If `x` is `Error []`, returns `Error defaultValue`. Otherwise, it forwards `x`.
    let defaultError (defaultValue: 'error list) (x: Validation<'value, 'error>): Validation<'value, 'error> =
        defaultErrorWith (fun () -> defaultValue) x

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
            member __.Zero () = Error []
            member __.Yield (_: unit): Validation<_, 'error> = Ok (unbox<_> null)

            member __.For (x: Validation<'value, 'error>, f: 'value -> Validation<_, 'error>) = bind f x
            member __.Combine (m: Validation<'value, 'error>, f: unit -> Validation<'value, 'error>): Validation<'value, 'error> =
                match m, f () with
                | Ok value, Error [] -> Ok value
                | Error [], Ok value -> Ok value
                | Ok value, Ok _ -> Ok value
                | Error xs, Error ys -> Error (xs @ ys)
                | _, Error ys -> Error ys
                | Error xs, _ -> Error xs

            [<CustomOperation ("validate", IsLikeZip = true)>]
            member __.Validate (outer: Validation<'outer, 'error>, inner: Validation<'inner, 'error>, f: 'outer -> 'inner -> 'result): Validation<'result, 'error> =
                f <!> outer <*> inner

            member __.Yield (x: 'error): Validation<'value, 'error> = Error [x]
            member __.YieldFrom (xs: 'error list): Validation<'value, 'error> = Error xs
            member __.YieldFrom (x:  Validation<_, 'error>) = x

            member __.Return (x: 'value): Validation<'value, 'error> = Ok x
            member __.ReturnFrom (x:  Validation<_, 'error>) = x

            member __.Delay x = x
            member __.Run x = x ()

        let validation<'error> = ValidationBuilder<'error> ()
