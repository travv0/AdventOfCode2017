open System.IO

type Dir =
    | NW
    | N
    | NE
    | SE
    | S
    | SW

    member dir.Clockwise() = dir.Clockwise(1)

    member dir.Clockwise(n) =
        let newDir =
            match dir with
            | NW -> N
            | N -> NE
            | NE -> SE
            | SE -> S
            | S -> SW
            | SW -> NW

        if n > 1 then
            newDir.Clockwise(n - 1)
        else
            newDir

    member dir.CounterClockwise() = dir.CounterClockwise(1)

    member dir.CounterClockwise(n) =
        let newDir =
            match dir with
            | SE -> NE
            | S -> SE
            | SW -> S
            | NW -> SW
            | N -> NW
            | NE -> N

        if n > 1 then
            newDir.CounterClockwise(n - 1)
        else
            newDir

    static member Parse =
        function
        | "nw" -> NW
        | "n" -> N
        | "ne" -> NE
        | "sw" -> SW
        | "s" -> S
        | "se" -> SE
        | s -> failwithf "bad parse: %s" s

let parse (input: string) =
    input.Trim().Split(',')
    |> Seq.map Dir.Parse
    |> List.ofSeq

module Array =
    let remove elem a =
        Array.tryFindIndex ((=) elem) a
        |> Option.map (fun i -> Array.removeAt i a)

    let replace elem with_ a =
        Array.tryFindIndex ((=) elem) a
        |> Option.map (fun i -> Array.updateAt i with_ a)

let reducePath path =
    List.fold
        (fun (path, furthest) (dir: Dir) ->
            let newPath =
                Array.remove (dir.Clockwise(3)) path
                |> Option.orElse (Array.replace (dir.Clockwise(2)) (dir.Clockwise()) path)
                |> Option.orElse (Array.replace (dir.CounterClockwise(2)) (dir.CounterClockwise()) path)
                |> Option.defaultValue (Array.append path [| dir |])

            let newFurthest = Array.length newPath |> max furthest
            (newPath, newFurthest))
        ([||], 0)
        path

let (path, furthestDistance) =
    File.ReadAllText("input.txt")
    |> parse
    |> reducePath

path
|> Array.length
|> printfn "The child process is %d steps away"

printfn "The furthest he ever got from his starting position is %d steps" furthestDistance
