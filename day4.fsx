open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine (line: string) =
    match line with
    | Regex @"([a-z\-]*)-(\d*)\[([a-z]*)\]" [ room; id; freq ] -> room, int id, freq
    | _ -> failwithf "invalid line '%s'" line

let freqFrom room =
    let rec loop (freq: Map<char, int>) cs =
        match cs with
        | '-' :: tail -> loop freq tail
        | c :: tail -> loop (freq.Add(c, 1 + (freq.TryFind c |> Option.defaultValue 0))) tail
        | [] -> freq

    loop Map.empty (Seq.toList room)

let top n (frequencies: Map<char, int>) =
    frequencies
    |> Map.toList
    |> List.sortBy (fun (c, o) -> o, -int c)
    |> List.rev
    |> List.take n
    |> List.map fst

let checkRoom (room, id, freq) =
    if (Seq.toList freq) = (room |> freqFrom |> top 5)
    then id
    else 0

File.ReadAllLines "input_4.txt"
|> Seq.sumBy (parseLine >> checkRoom)

let shiftBy n c =
    let c' = int c + n % 26
    if c' >= int 'z' then char (c' - 26) else char c'

let shiftStringBy n s =
    let rec loop acc cs =
        match cs with
        | '-' :: tail -> loop ('-' :: acc) tail
        | c :: tail -> loop ((shiftBy n c) :: acc) tail
        | [] ->
            List.rev acc
            |> Array.ofList
            |> System.String.Concat

    loop [] (Seq.toList s)

let handleLine (room, id, freq) =
    match checkRoom (room, id, freq) with
    | 0 -> "", 0
    | x -> (shiftStringBy id room), id

File.ReadAllLines "input_4.txt"
|> Seq.map (parseLine >> handleLine)
|> Seq.filter (fun (x, _) -> x.Contains "storage")
|> Seq.toList

"h"
