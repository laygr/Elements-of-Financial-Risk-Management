﻿namespace RiskManagement

module Workshop4 =
    open Deedle
    open System
    open MathNet.Numerics.Statistics
    open Core
    open RiskManagement.Workshops

    let w4_1 (frame:Frame<int,string>) =
        let data = Common.loadFrameFromCsv "..\..\Workshop2 data.csv" "Date"
        data?Return <- Series.returns Logarithmic Long (Frame.getCol "IPC" data)

        let returns = 
            Frame.getCol "Return" data
            |> Series.values
            |> Array.ofSeq
        0.0

        //Models.example2()
        //Models.garch returns 0.0004

        //let garch = new GARCH(returns, float32 0.0004)
        //garch.Solve()