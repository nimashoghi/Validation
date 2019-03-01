module Validation.Operators

module Validation =
    open Validation.Validation

    /// **Description**
    ///     Validation map operator
    let (<!>) f x = map f x

    /// **Description**
    ///     Validation apply operator
    let (<*>) f x = apply f x

    /// **Description**
    ///     Validation bind operator
    let (>>=) x f = bind f x

module AsyncValidation =
    open Validation.AsyncValidation

    /// **Description**
    ///     Validation map operator
    let (<!>) f x = map f x

    /// **Description**
    ///     Validation apply operator
    let (<*>) f x = apply f x

    /// **Description**
    ///     Validation bind operator
    let (>>=) x f = bind f x
