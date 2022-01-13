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

module Array =
    let remove elem a =
        Array.tryFindIndex ((=) elem) a
        |> Option.map (fun i -> Array.removeAt i a)

    let replace elem with_ a =
        Array.tryFindIndex ((=) elem) a
        |> Option.map (fun i -> Array.updateAt i with_ a)

let reducePath path =
    List.fold
        (fun (path, furthest) dir ->
            let newPath =
                match dir with
                | NE ->
                    Array.remove SW path
                    |> Option.orElseWith (fun () -> Array.replace NW N path)
                    |> Option.orElseWith (fun () -> Array.replace S SE path)
                | SE ->
                    Array.remove NW path
                    |> Option.orElseWith (fun () -> Array.replace SW S path)
                    |> Option.orElseWith (fun () -> Array.replace N NE path)
                | NW ->
                    Array.remove SE path
                    |> Option.orElseWith (fun () -> Array.replace NE N path)
                    |> Option.orElseWith (fun () -> Array.replace S SW path)
                | SW ->
                    Array.remove NE path
                    |> Option.orElseWith (fun () -> Array.replace SE S path)
                    |> Option.orElseWith (fun () -> Array.replace N NW path)
                | N ->
                    Array.remove S path
                    |> Option.orElseWith (fun () -> Array.replace SW NW path)
                    |> Option.orElseWith (fun () -> Array.replace SE NE path)
                | S ->
                    Array.remove N path
                    |> Option.orElseWith (fun () -> Array.replace NW SW path)
                    |> Option.orElseWith (fun () -> Array.replace NE SE path)
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
