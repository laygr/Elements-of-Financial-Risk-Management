namespace RiskManagement

module NonnormalDistributions =
    open MathNet.Numerics.Distributions
    open Utils
    open FSharp.Charting
    open System
    open FSharp.Charting

    // P. 124
    let qqPlot (standarizedReturns:float seq) =
        let sorted = Seq.sort standarizedReturns
        let n = Seq.length standarizedReturns |> float

        let normal = new Normal(0.0, 1.0)
        
        let normalQuantile = 
            [1. .. n]
            |> Seq.map
                ((fun i -> (i - 0.5)/n)
                >> normal.InverseCumulativeDistribution)

        let returnPoints = 
            Seq.zip normalQuantile sorted
            |> Seq.map (fun (normali, zi) -> (normali, zi))

        let normalPoints =
            [-8.0 .. 1.0 .. 8.0]
            |> Seq.map (fun x -> x, x)
            
        [Chart.Line(returnPoints, Name="returns", Color=Drawing.Color.RoyalBlue)
         Chart.Line(normalPoints, Name="normal", Color=Drawing.Color.Red)]
        |> Chart.Combine
        |> Chart.WithXAxis(Title="Normal quantile", Max=8.0, Min=(-8.0), MinorTickMark=ChartTypes.TickMark(Interval=1.0))
        |> Chart.WithYAxis(Title="Return quantile", Max=8.0, Min=(-8.0), MinorTickMark=ChartTypes.TickMark(Interval=1.0))
        |> Chart.WithTitle "QQ Plot"
        |> Chart.Show 

    // P. 125
    let standarizeReturns returns variance =
        Seq.zip returns variance
        |> Seq.map (fun (ret,var) -> ret/(sqrt var))