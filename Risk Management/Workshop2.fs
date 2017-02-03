module Workshop2
open Deedle
open System
open MathNet.Numerics.Statistics
open RiskM

// Workshops are not from the book but the homework given by the professor of a class given at ITAM (https://www.itam.mx)
(*
Workshop 2
Each month a company computes the value at risk for the next month as the minimum return of that month times the square root of 22.
For example, the VaR for January 2008 is the minimum daily return of December 2007 times the square root of 22.
The trading limit is $100,000 and the trader starts its trading on December 31, 2007 when he has a VaR for January and he can go long the IPC index.
*)

(*
W2 3.- a)
What is the 1-month 5%-VaR for January 2008?
Compute it using monthly returns,
using RiskMetrics with a λ=0.94,
for the first trading day of January 2008
*)
let w2_3_a =
    let data =
        Frame.ReadCsv("..\..\Workshop2 data.csv")
        |> Frame.indexRowsUsing toDateTime
        |> Frame.sortRowsByKey
    data?Return <- returnWhenLonging data

    let monthlyReturns = monthlyReturns (data?Return)
    let sampleVariance = Statistics.Variance(monthlyReturns.Values)

    let monthlyVariances =
        monthlyReturns
        |> RiskMetrics 0.94 sampleVariance
       
    let varForMonth (month:DateTime) =
        monthlyVariances.[month]
        |> Math.Sqrt
        |> var 0.95 0.

    let january2008 = DateTime(2008,01,01)
    varForMonth january2008
    

(*
W2 4.- a)
Compute de 5% Expected Shortfall for January 2008 using Historical Simulation 250 daily returns;
do not forget to multiply it by sqrt 22.
*)

let w2_4_a =
    let data =
        Frame.ReadCsv("..\..\Workshop2 data.csv")
        |> Frame.indexRowsUsing toDateTime
        |> Frame.sortRowsByKey    
    data?Return <- returnWhenLonging data

    let ``08/01/02`` = DateTime(2008,01,02)                             // First trading date of January 2008
    let simulationData = (nRowsBeforeKey 250 data ``08/01/02``)?Return
    let VaR = historicalSimulation 0.95 simulationData

    empiricalExpectedShortfall VaR simulationData                       // January 02 2008 expected shortfall
    |> (*) (Math.Sqrt 22.)                                              // January 2008 expected shortfall

(*
W2 4.- b)
Compute de 5% Expected Shortfall for January 2008 using as VaR the VaR obtained in W2 3.- a)
*)
let w2_4_b =
    let VaR = w2_3_a
    let stdDev = stdDevFromVaR 0.95 VaR
    expectedShortfall 0.95 stdDev

let workshop2 () =
    printfn "3.- a) %f" w2_3_a
    printfn "4.- a) %f" w2_4_a
    printfn "4.- b) %f" w2_4_b