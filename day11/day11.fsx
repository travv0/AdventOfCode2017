open System.IO

type Dirs =
    | NW
    | N
    | NE
    | SW
    | S
    | SE

let parse (input: string) =
    input.Trim().Split(',')
    |> Seq.map (function
        | "nw" -> NW
        | "n" -> N
        | "ne" -> NE
        | "sw" -> SW
        | "s" -> S
        | "se" -> SE
        | s -> failwithf "bad parse: %s" s)
    |> List.ofSeq

module List =
    let remove elem l =
        List.tryFindIndex ((=) elem) l
        |> Option.bind (fun i ->
            match List.splitAt i l with
            | (beginning, _ :: ending) -> Some(beginning @ ending)
            | _ -> None)

    let replace elem with_ l =
        List.tryFindIndex ((=) elem) l
        |> Option.bind (fun i ->
            match List.splitAt i l with
            | (beginning, _ :: ending) -> Some(beginning @ with_ :: ending)
            | _ -> None)

let reducePath path =
    (Array.ofList path, ([], 0))
    ||> Array.foldBack (fun dir (path, furthest) ->
        let newPath =
            match dir with
            | NE ->
                List.remove SW path
                |> Option.orElseWith (fun () -> List.replace NW N path)
                |> Option.orElseWith (fun () -> List.replace S SE path)
                |> Option.defaultValue (dir :: path)
            | SE ->
                List.remove NW path
                |> Option.orElseWith (fun () -> List.replace SW S path)
                |> Option.orElseWith (fun () -> List.replace N NE path)
                |> Option.defaultValue (dir :: path)
            | NW ->
                List.remove SE path
                |> Option.orElseWith (fun () -> List.replace NE N path)
                |> Option.orElseWith (fun () -> List.replace S SW path)
                |> Option.defaultValue (dir :: path)
            | SW ->
                List.remove NE path
                |> Option.orElseWith (fun () -> List.replace SE S path)
                |> Option.orElseWith (fun () -> List.replace N NW path)
                |> Option.defaultValue (dir :: path)
            | N ->
                List.remove S path
                |> Option.orElseWith (fun () -> List.replace SW NW path)
                |> Option.orElseWith (fun () -> List.replace SE NE path)
                |> Option.defaultValue (dir :: path)
            | S ->
                List.remove N path
                |> Option.orElseWith (fun () -> List.replace NW SW path)
                |> Option.orElseWith (fun () -> List.replace NE SE path)
                |> Option.defaultValue (dir :: path)

        let newFurthest = List.length newPath |> max furthest
        (newPath, newFurthest))

let path = File.ReadAllText("input.txt") |> parse
let (shortestPath, furthestDistance) = reducePath path

shortestPath
|> List.length
|> printfn "The child process is %d steps away"

printfn "The furthest he ever got from his starting position is %d steps" furthestDistance
