open System.IO

type Instruction =
    | L of int
    | R of int

type Dir =
    | N
    | S
    | W
    | E

let turnLeft dir steps x y =
    match dir with
    | N -> W, x - steps, y
    | W -> S, x, y - steps
    | S -> E, x + steps, y
    | E -> N, x, y + steps

let turnRight dir steps x y =
    match dir with
    | S -> W, x - steps, y
    | E -> S, x, y - steps
    | N -> E, x + steps, y
    | W -> N, x, y + steps

let parseElement (element: string) =
    match element.[0] with
    | 'R' -> R(int element.[1..])
    | 'L' -> L(int element.[1..])
    | _ -> failwith (sprintf "invalid element '%s'" element)

let instructions =
    (File.ReadAllText "input_1.txt").Split ","
    |> Array.map (fun s -> parseElement (s.Trim()))

let rec move (dir, x, y) instructions =
    match instructions with
    | next :: tail ->
        match next with
        | L n -> move (turnLeft dir n x y) tail
        | R n -> move (turnRight dir n x y) tail
    | [] -> x, y

move (N, 0, 0) (Array.toList instructions)

let rec moveUnique dir x y seen instructions =
    match instructions with
    | [] -> failwith "no location visited twice"
    | next :: tail ->
        let (dir', (dx, dy), steps) =
            match next with
            | L n ->
                match dir with
                | N -> W, (-1, 0), n
                | W -> S, (0, -1), n
                | S -> E, (1, 0), n
                | E -> N, (0, 1), n
            | R n ->
                match dir with
                | S -> W, (-1, 0), n
                | E -> S, (0, -1), n
                | N -> E, (1, 0), n
                | W -> N, (0, 1), n

        let mutable newPoints = Set.empty
        for i in 1 .. steps do
            newPoints <- newPoints.Add(x + i * dx, y + i * dy)
        if Set.isEmpty (Set.intersect newPoints seen)
        then moveUnique dir' (x + steps * dx) (y + steps * dy) (Set.union newPoints seen) tail
        else Set.intersect newPoints seen

moveUnique N 0 0 (Set.empty.Add(0, 0)) (Array.toList instructions)
