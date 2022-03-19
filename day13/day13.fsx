open System.IO

type Direction =
    | Up
    | Down

type Layer =
    { Depth: int
      Range: int
      ScannerPosition: int
      ScannerDirection: Direction }
    member this.Severity = this.Depth * this.Range

type Firewall =
    { Layers: option<Layer> []
      YourPosition: int
      CaughtLayers: list<Layer> }

let parseLayers (input: string []) =
    input
    |> Array.map
        (fun line ->
            match line.Split(": ") |> Array.map int with
            | [| depth; range |] ->
                { Depth = depth
                  Range = range
                  ScannerPosition = 1
                  ScannerDirection = Down }
            | _ -> failwith "bad parse")

let makeFirewall layers =
    let firewallLayers =
        Array.create
            ((layers
              |> Array.map (fun layer -> layer.Depth)
              |> Array.max)
             + 1)
            None

    for layer in layers do
        firewallLayers.[layer.Depth] <- Some layer

    { Layers = firewallLayers
      YourPosition = -1
      CaughtLayers = [] }

let updateLayers firewall =
    firewall.Layers
    |> Array.map
        (fun layer ->
            layer
            |> Option.map
                (fun layer ->
                    match layer.ScannerDirection with
                    | Up ->
                        if layer.ScannerPosition > 1 then
                            { layer with
                                  ScannerPosition = layer.ScannerPosition - 1 }
                        else
                            { layer with
                                  ScannerPosition = layer.ScannerPosition + 1
                                  ScannerDirection = Down }
                    | Down ->
                        if layer.ScannerPosition < layer.Range then
                            { layer with
                                  ScannerPosition = layer.ScannerPosition + 1 }
                        else
                            { layer with
                                  ScannerPosition = layer.ScannerPosition - 1
                                  ScannerDirection = Up }))

let step (firewall: Firewall) : Firewall =
    let newYourPosition = firewall.YourPosition + 1

    let newCaughtLayer =
        firewall.Layers.[newYourPosition]
        |> Option.bind
            (fun layer ->
                if layer.ScannerPosition = 1 then
                    Some layer
                else
                    None)

    let newLayers = updateLayers firewall

    { Layers = newLayers
      YourPosition = newYourPosition
      CaughtLayers =
          firewall.CaughtLayers
          @ Option.toList newCaughtLayer }

let rec runTrip firewall =
    if firewall.YourPosition
       <> firewall.Layers.Length - 1 then
        step firewall |> runTrip
    else
        firewall.CaughtLayers

let input = File.ReadAllLines("input.txt")
let firewall = input |> parseLayers |> makeFirewall

let testFirewall =
    [| "0: 3"; "1: 2"; "4: 4"; "6: 4" |]
    |> parseLayers
    |> makeFirewall

runTrip firewall
|> List.sumBy (fun layer -> layer.Severity)
|> printfn "The severity of your whole trip is %d"

let rec isCaught firewall =
    if firewall.YourPosition
       <> firewall.Layers.Length - 1 then
        let newFirewall = step firewall

        if newFirewall.CaughtLayers.Length > 0 then
            true
        else
            isCaught newFirewall
    else
        false

let findDelayToNotGetCaught firewall =
    let rec loop firewall delay =
        if isCaught firewall then
            loop
                { firewall with
                      Layers = updateLayers firewall }
                (delay + 1)
        else
            delay

    loop firewall 0

findDelayToNotGetCaught firewall
|> printfn
    "The fewest number of picoseconds you need to delay the packet to not get caught is %d"
