namespace RiskManagement

module Utils =

    let sumProduct (a:float seq) (b:float seq) =
        Seq.zip a b
        |> Seq.map (fun (a', b') -> a' * b')
        |> Seq.sum