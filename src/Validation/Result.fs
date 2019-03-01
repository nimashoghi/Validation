module Validation.Validation

/// **Description**
///     Applies the function `f` to the value inside container `x`.
let map (f: _ -> _) (x: Result<_, _ list>): Result<_, _ list> = Result.map f x

/// **Description**
///     Applies the function `f` to the error inside container `x`.
let mapError (f: _ -> _) (x: Result<_, _ list>): Result<_, _ list> = Result.mapError f x

/// **Description**
///     Applies the function `f` to the error inside container `x` and flattens the result.
let bind (f: _ -> Result<_, _ list>) (x: Result<_, _ list>): Result<_, _ list> = Result.bind f x

/// **Description**
///     Applies the function inside the container `f` to the value inside the container `x`.
let apply (f: Result<_ -> _, _ list>) (x: Result<_, _ list>): Result<_, _ list> =
    match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error lhs, Error rhs -> Error (lhs @ rhs)
    | Error lhs, _ -> Error lhs
    | _, Error rhs -> Error rhs

/// **Description**
///     Converts `x` to an option.
let toOption (x: Result<'value, 'error list>): 'value option =
    match x with
    | Ok value -> Some value
    | _ -> None

/// **Description**
///     Converts `x` to a value option.
let toValueOption (x: Result<'value, 'error list>): 'value voption =
    match x with
    | Ok value -> ValueSome value
    | _ -> ValueNone

/// **Description**
///     Converts `x` from a `'value option` to a `Result<'value, 'error list>`, returning the result of `error ()` if `x` is `None`.
let ofOptionWith (error: unit -> 'error list) (x: 'value option): Result<'value, 'error list> =
    match x with
    | Some value -> Ok value
    | None -> Error (error ())

/// **Description**
///     Converts `x` from a `'value option` to a `Result<'value, 'error list>`, returning `error` if `x` is `None`.
let ofOption (error: 'error list) (x: 'value option): Result<'value, 'error list> =
    ofOptionWith (fun () -> error) x

/// **Description**
///     Converts `x` from a `'value voption` to a `Result<'value, 'error list>`, returning the result of `error ()` if `x` is `ValueNone`.
let ofValueOptionWith (error: unit -> 'error list) (x: 'value voption): Result<'value, 'error list> =
    match x with
    | ValueSome value -> Ok value
    | ValueNone -> Error (error ())

/// **Description**
///     Converts `x` from a `'value voption` to a `Result<'value, 'error list>`, returning `error` if `x` is `ValueNone`.
let ofValueOption (error: 'error list) (x: 'value voption): Result<'value, 'error list> =
    ofValueOptionWith (fun () -> error) x

/// **Description**
///     Calls `f` on the value inside of `x`. If the result is `Some`, it returns that value. Otherwise, returns the result of `error ()`.
let tryPickWith (error: unit -> 'error list) (f: 'value -> 'result option) (x: Result<'value, 'error list>): Result<'result, 'error list> =
    bind (f >> ofOptionWith error) x

/// **Description**
///     Calls `f` on the value inside of `x`. If the result is `Some`, it returns that value. Otherwise, returns `error`.
let tryPick (error: 'error list) (f: 'value -> 'result option) (x: Result<'value, 'error list>): Result<'result, 'error list> =
    tryPickWith (fun () -> error) f x

/// **Description**
///     Calls `f` on the value inside of `x`. If the result is `true`, it returns that value. Otherwise, returns `error`.
let tryFind (error: 'error list) (f: 'value -> bool) (x: Result<'value, 'error list>): Result<'value, 'error list> =
    tryPickWith (fun () -> error) (fun value -> if f value then Some value else None) x

/// **Description**
///     Returns `true` if the value inside of `x` is equal to `value`.
let contains (value: 'value) (x: Result<'value, 'error list>): bool =
    match x with
    | Ok value' when value' = value -> true
    | _ -> false

/// **Description**
///     Flattens the nested validation object `x` of type `Result<Validation<'value, > list>` into a `Result<'value, 'error list>`
let flatten (x: Result<Result<'value, 'error list>, 'error list>): Result<'value, 'error list> = bind id x

/// **Description**
///     If `x` is `Error []`, returns the result of calling `f`. Otherwise, it forwards `x`.
let defaultWith (f: unit -> Result<'value, 'error list>) (x: Result<'value, 'error list>): Result<'value, 'error list> =
    match x with
    | Ok value -> Ok value
    | Error [] -> f ()
    | Error err -> Error err

/// **Description**
///     If `x` is `Error []`, returns `defaultValue`. Otherwise, it forwards `x`.
let defaultValue (defaultValue: Result<'value, 'error list>) (x: Result<'value, 'error list>): Result<'value, 'error list> =
    defaultWith (fun () -> defaultValue) x

/// **Description**
///     If `x` is `Error []`, returns the result of calling `f >> Ok`. Otherwise, it forwards `x`.
let defaultSuccessWith (f: unit -> 'value) (x: Result<'value, 'error list>): Result<'value, 'error list> =
    defaultWith (f >> Ok) x

/// **Description**
///     If `x` is `Error []`, returns `Ok defaultValue`. Otherwise, it forwards `x`.
let defaultSuccess (defaultValue: 'value) (x: Result<'value, 'error list>): Result<'value, 'error list> =
    defaultSuccessWith (fun () -> defaultValue) x

/// **Description**
///     If `x` is `Error []`, returns the result of calling `f >> Error`. Otherwise, it forwards `x`.
let defaultErrorWith (f: unit -> 'error list) (x: Result<'value, 'error list>): Result<'value, 'error list> =
    defaultWith (f >> Error) x

/// **Description**
///     If `x` is `Error []`, returns `Error defaultValue`. Otherwise, it forwards `x`.
let defaultError (defaultValue: 'error list) (x: Result<'value, 'error list>): Result<'value, 'error list> =
    defaultErrorWith (fun () -> defaultValue) x
