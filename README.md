# Validation [![Build Status](https://travis-ci.org/nimashoghi/Validation.svg?branch=master)](https://travis-ci.org/nimashoghi/Validation)

## Example Usage

First, we create some basic validation functions using the existing `Result<_, _>` type in F#.

Note that the `Error` case returns a `string list`. This is necessary.

```fs
let validateAge (age: int) =
    if age = 21
    then Ok 21
    else Error ["Invalid Age"]

let validateHeight (height: float) =
    if height = 180.
    then Ok 180.
    else Error ["Invalid Height"]

let validateName (name: string) =
    if name = "Alice"
    then Ok "Alice"
    else Error ["Invalid Name"]
```

Then, we create a composite data type that we want to validate.

```fs
type Data = {
    Age: int
    Height: float
    Name: string
    NotValidated: string
}
```

Now, we can use our `validation` computation expression to compose our existing validators.

```fs
let validate (data: Data) =
    validation {
        validate age in validateAge data.Age
        validate height in validateHeight data.Height
        validate name in validateName data.Name

        return {
            data with
                Age = age
                Height = height
                Name = name
        }
    }
```

Take a look at sample.fsx or our unit tests for more examples.
