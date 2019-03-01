let list = [|
    for i in 0 .. 100000 do yield i
|]

let rec map f (acc: _ []) (lst: _ []) =
    match lst with
    | [] -> acc
    | head :: tail ->
        map f (f head :: acc) tail

map ((+) 5) [] list
