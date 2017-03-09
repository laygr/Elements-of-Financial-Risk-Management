namespace RiskManagement

module Workshop4 =
    open Deedle
    open System
    open MathNet.Numerics.Statistics
    open Core

    let w4_1 (frame:Frame<int,string>) =
        let data =
            frame
            |> Frame.indexRowsUsing toDateTime
            |> Frame.sortRowsByKey
        data?Return <- returnWhenLonging data "IPC"
        let returns = 
            Frame.getCol "Return" data
            |> Series.values
            |> Array.ofSeq
        0.0

        //Models.example2()
        //Models.garch returns 0.0004

        //let garch = new GARCH(returns, float32 0.0004)
        //garch.Solve()