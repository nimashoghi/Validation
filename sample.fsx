let validateAge (input: int) =
    if input = 21
    then Ok 21
    else Error ["Invalid Age"]

let validateHeight (input: float) =
    if input = 180.
    then Ok 180.
    else Error ["Invalid Height"]

let validateName (input: string) =
    if input = "Alice"
    then Ok "Alice"
    else Error ["Invalid Name"]

type Data = {
    Age: int
    Height: float
    Name: string
    NotValidated: string
}

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

validate {
    Age = 20
    Height = 181.
    Name = "Alice"
    NotValidated = "hello"
}
