open System.IO

module List =
    let rotatel n l =
        l
        |> List.splitAt (n % List.length l)
        ||> (fun after before -> List.append before after)

let list = [ 0 .. 255 ]

let runRound lengths =
    let rec go lengths currentPos skipSize list =
        match lengths with
        | l :: ls ->
            let (toReverse, after) = List.splitAt l list

            let newList =
                toReverse
                |> Seq.rev
                |> Seq.append after
                |> Seq.toList
                |> List.rotatel skipSize

            go ls ((currentPos + l + skipSize) % List.length list) (skipSize + 1) newList
        | _ -> List.rotatel (List.length list - currentPos) list

    go lengths 0 0 list

let runRounds n lengths =
    Seq.replicate n lengths |> List.concat |> runRound

let hash (input: string) =
    let lengths =
        input.Trim() |> Seq.map int |> Seq.append
        <| [ 17; 31; 73; 47; 23 ]
        |> List.ofSeq

    let sparseHash = runRounds 64 lengths

    let denseHash =
        List.chunkBySize 16 sparseHash
        |> List.map (List.reduce (^^^))

    List.map (sprintf "%02x") denseHash
    |> String.concat ""


module Part1 =
    let lengths =
        File.ReadAllText("input.txt").Trim().Split(',')
        |> Seq.map int
        |> List.ofSeq

    match runRound lengths with
    | (a :: b :: _) -> printfn "Part 1: %d" (a * b)
    | _ -> failwith "part 1 failed"

module Part2 =
    File.ReadAllText("input.txt")
    |> hash
    |> printfn "Part 2: %s"
