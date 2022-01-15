open System.IO

let flip f a b = f b a

module List =
    let rotatel n l =
        l
        |> List.splitAt (n % List.length l)
        ||> flip List.append

let runRound lengths =
    let rec go currentPos skipSize list =
        function
        | l :: ls ->
            let (toReverse, after) = List.splitAt l list

            let newList =
                toReverse
                |> Seq.rev
                |> Seq.append after
                |> Seq.toList
                |> List.rotatel skipSize

            go ((currentPos + l + skipSize) % List.length list) (skipSize + 1) newList ls
        | _ -> List.rotatel (List.length list - currentPos) list

    go 0 0 [ 0 .. 255 ] lengths

let runRounds n lengths =
    Seq.replicate n lengths |> List.concat |> runRound

let hash (input: string) =
    let lengths =
        input.Trim()
        |> Seq.map int
        |> (flip Seq.append [ 17; 31; 73; 47; 23 ])
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
