open System.IO

type Dir =
    | U
    | D
    | L
    | R

let parseDir c =
    match c with
    | 'U' -> U
    | 'D' -> D
    | 'L' -> L
    | 'R' -> R
    | _ -> failwith "wrong char"

let parseLine (l: string) = l |> Seq.map parseDir

let moves =
    File.ReadAllLines "input_2.txt"
    |> Seq.map parseLine

let keypad1 at dir =
    match dir with
    | U -> if at = 1 || at = 2 || at = 3 then at else at - 3
    | D -> if at = 7 || at = 8 || at = 9 then at else at + 3
    | L -> if at = 1 || at = 4 || at = 7 then at else at - 1
    | R -> if at = 3 || at = 6 || at = 9 then at else at + 1

let code keypad start moves =
    let rec lineLoop pos moves =
        match moves with
        | next :: tail -> lineLoop (keypad pos next) tail
        | [] -> pos

    let rec codeLoop acc pos moves =
        match moves with
        | next :: tail ->
            let nextCode = lineLoop pos (Seq.toList next)
            codeLoop (nextCode :: acc) nextCode tail
        | [] -> List.rev acc

    codeLoop [] start (Seq.toList moves)

// part 1
code keypad1 5 moves

let keypad2 at dir =
    match dir with
    | U ->
        match at with
        | 3 -> 1
        | 6
        | 7
        | 8
        | 10
        | 11
        | 12 -> at - 4
        | 13 -> 11
        | _ -> at
    | D ->
        match at with
        | 1 -> 3
        | 2
        | 3
        | 4
        | 6
        | 7
        | 8 -> at + 4
        | 11 -> 13
        | _ -> at
    | L ->
        match at with
        | 3
        | 4
        | 6
        | 7
        | 8
        | 9
        | 11
        | 12 -> at - 1
        | _ -> at
    | R ->
        match at with
        | 2
        | 3
        | 5
        | 6
        | 7
        | 8
        | 10
        | 11 -> at + 1
        | _ -> at

// part 2
code keypad2 5 moves
