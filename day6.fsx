open System.IO

let incr (counter: Map<char, int>) c =
    counter.Add(c, 1 + (counter.TryFind c |> Option.defaultValue 0))

let max (counter: Map<char, int>) =
    counter |> Map.toList |> List.maxBy snd |> fst

let min (counter: Map<char, int>) =
    counter |> Map.toList |> List.minBy snd |> fst

let parse lines =
    let mutable counters = Array.init 8 (fun _ -> Map.empty)

    let rec pLine lines =
        match lines with
        | line :: tail ->
            line
            |> Seq.toList
            |> Seq.indexed
            |> Seq.iter (fun (i, c) -> counters.[i] <- incr counters.[i] c)
            pLine tail
        | [] -> ()

    pLine (Array.toList lines)
    counters

// part 1
File.ReadAllLines "input_6.txt"
|> parse
|> Array.map max
// part 2
File.ReadAllLines "input_6.txt"
|> parse
|> Array.map min
