open System.IO

let parseLine (line: string) =
    let rec loop (outs, ins, cur) isIn cs =
        match cs with
        | '[' :: tail -> loop ((List.rev cur) :: outs, ins, []) true tail
        | ']' :: tail -> loop (outs, (List.rev cur) :: ins, []) false tail
        | c :: tail -> loop (outs, ins, c :: cur) isIn tail
        | [] -> (List.rev cur) :: outs, ins

    loop ([], [], []) false (Seq.toList line)

let testCharList (cs: char list) =
    let check c1 c2 cs =
        match cs with
        | c2' :: c1' :: _ when c2 = c2' && c1 = c1' -> true
        | _ -> false

    let rec loop last cs =
        match cs with
        | head :: tail when head = last -> loop head tail
        | next :: tail ->
            match check last next tail with
            | true -> true
            | false -> loop next tail
        | [] -> false

    loop cs.[0] cs.[1..]

let testCharlists css =
    let rec loop css =
        match css with
        | cs :: tail ->
            match testCharList cs with
            | true -> true
            | false -> loop tail
        | [] -> false

    loop css

File.ReadAllLines "input_7.txt"
|> Seq.map
    (parseLine
     >> (fun (x, y) -> testCharlists x && not (testCharlists y)))
|> Seq.countBy id

let allAreaBroadcastAccessor (cs: char list) =
    let check c1 c2 cs =
        match cs with
        | c1' :: _ when c1 = c1' -> true
        | _ -> false

    let rec loop (acc: Set<char * char>) last cs =
        match cs with
        | head :: tail when head = last -> loop acc head tail
        | next :: tail ->
            match check last next tail with
            | true -> loop (acc.Add(last, next)) next tail
            | false -> loop acc next tail
        | [] -> acc

    loop Set.empty cs.[0] cs.[1..]

let checkLine (outers, inners) =
    let abas =
        outers
        |> List.map allAreaBroadcastAccessor
        |> List.fold Set.union Set.empty

    let babs =
        inners
        |> List.map allAreaBroadcastAccessor
        |> List.fold Set.union Set.empty
        |> Set.toList

    let rec any babs =
        match babs with
        | (b, a) :: tail ->
            match abas.Contains(a, b) with
            | true -> true
            | false -> any tail
        | [] -> false

    any babs

File.ReadAllLines "input_7.txt"
|> Seq.map (parseLine >> checkLine)
|> Seq.countBy id
