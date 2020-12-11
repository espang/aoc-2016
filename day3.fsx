open System.IO

let testTriangle coll =
    let tri = List.sort coll
    tri.[0] + tri.[1] > tri.[2]

let validLine (s: string) =
    try
        let items =
            s.Split()
            |> Array.filter (fun x -> x <> "")
            |> Array.map (fun x -> int (x.Trim()))
            |> Array.sort
            |> Array.toList

        testTriangle items
    with _ -> false

File.ReadAllLines "input_3.txt"
|> Seq.filter validLine
|> Seq.length

let rec testList acc coll =
    if List.isEmpty coll
    then acc
    else testList (if testTriangle (List.take 3 coll) then acc + 1 else acc) (List.skip 3 coll)

let handleColumn n coll =
    coll
    |> Seq.indexed
    |> Seq.filter (fun (idx, v) -> idx % 3 = n)
    |> Seq.map snd
    |> Seq.toList
    |> testList 0

let allNumberes =
    File.ReadAllLines "input_3.txt"
    |> Seq.collect (fun l ->
        l.Split()
        |> Seq.filter (fun x -> x <> "")
        |> Seq.map int)

handleColumn 0 allNumberes
+ handleColumn 1 allNumberes
+ handleColumn 2 allNumberes
