open System.Security.Cryptography
open System.Text

let input = "abbhdwsy"

let md5 (data: byte array): string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let findNextNumber number (pred: string -> bool) input =
    let rec _next number =
        let hash =
            md5 (Encoding.ASCII.GetBytes(input + (string number)))

        let test = pred hash
        match test with
        | true -> number, hash.[5], hash.[6]
        | false -> _next (number + 1)

    _next number

let findPassword input =
    let pred (x: string) = x.StartsWith "00000"

    let rec loop password steps nbr =
        if steps = 0 then
            password |> List.rev
        else
            let nbr, c, _ = findNextNumber nbr pred input
            printfn "found %A" c
            loop (c :: password) (steps - 1) (nbr + 1)

    loop [] 8 1

findPassword input
|> Array.ofList
|> System.String.Concat

let findPassword2 input =
    let pred (x: string) = x.StartsWith "00000"
    let index (c: char) = int c - 48
    let mutable arr = Array.zeroCreate 8

    let rec loop (steps: Set<int>) nbr =
        if steps.Count = 8 then
            arr
        else
            let nbr, pos, c = findNextNumber nbr pred input
            let position = index pos
            if steps.Contains position || position > 7 then
                loop steps (nbr + 1)
            else
                arr.[position] <- c
                printfn "found %A" c
                loop (steps.Add(position)) (nbr + 1)

    loop Set.empty 1

findPassword2 input |> System.String.Concat
