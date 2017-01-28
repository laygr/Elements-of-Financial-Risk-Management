module RiskManagement

open Deedle
open System
open MathNet.Numerics.Statistics

(*
let toDateTime (os:ObjectSeries<_>) =
    let year = (os.Get "Year") :?> int
    let month = (os.Get "Month" :?> int)
    let day = (os.Get "Day" :?> int)
    DateTime(year,month,day)
*)

let toDateTime (os:ObjectSeries<_>) =
    (os.Get "Date" :?> string)
    |> DateTime.Parse

let returnWhenLonging (frame:Frame<DateTime,string>) =
    log frame?Close - log (Frame.shift 1 frame)?Close

let returnWhenShorting (frame:Frame<DateTime,string>) =
    log (Frame.shift 1 frame)?Close - log (frame?Close)

// page 12
let RiskMetrics lambda startingVariance (series:Series<DateTime,float>) =
    let next pastVariance' pastReturn' =
        match pastVariance', pastReturn' with
        | Some pastVariance, Some pastReturn -> Some (lambda * pastVariance + (1.-lambda) * pastReturn ** 2.)
        | _ -> pastVariance'

    Series.shift 1 series
    |> Series.scanAllValues next (Some startingVariance)

let nDaysBefore days (data:Frame<_,_>) day =
    data.GetSubrange(None, Some (day, Indices.Exclusive))
    |> Frame.takeLast days

let positionForKey (frame:Frame<'K,_>) (key:'K) =
    frame.RowKeys
    |> Seq.findIndex (fun k -> k = key)
    |> (+) 1

let nRowsBeforeKey rows (data:Frame<_,_>) key =
    data
    |> Frame.take ((positionForKey data key)-1)
    |> Frame.takeLast rows

let mapKey (f:'a -> 'b) (frame:Frame<_,_>) =
    Series.map (fun k _ -> f k) (frame.GetColumnAt 0)

let historicalSimulation confidence =
    Series.values
    >> fun v -> Statistics.quantileCustomFunc v QuantileDefinition.Excel
    >> (|>) (1. - confidence)
    >> (*) -1.

let weightedHistoricalSimulation confidence eta days (data:Frame<DateTime,_>) day =
    let subframe = nRowsBeforeKey days (data.Clone()) day

    let taus = Series.scanValues (fun counter next -> counter - 1) (days+1) (subframe.GetColumnAt(0))
    subframe?Tau <- taus
    
    let weight tau =
      eta**(float(tau - 1)) * (1.-eta)/(1.-eta ** (float days))

    let toWeight (os:ObjectSeries<_>) =
        let tau = (os.Get "Tau") :?> int
        weight tau

    subframe?Weight <- Frame.mapRowValues toWeight subframe

    let sorted = subframe.SortRowsBy("Return",fun i -> i)                               // sorted by returns
    sorted?AccumWeight <- Series.scanValues (fun accum w -> accum + w) 0. sorted?Weight    // create column of accumulated weights

    let filtered =                                                                      // days with accum weight smaller than (1-confidence)
        sorted
        |> Frame.filterRows (fun _ row -> row?AccumWeight < (1. - confidence))

    if filtered.RowCount = 0
    then
        sorted
        |> Frame.getCol("Return")
        |> Series.firstValue           // return the first day if no day has acumm weight smaller than (1-confidence)
    else
        filtered
        |> Frame.getCol("Return")
        |> Series.lastValue           // else, return the last day of the days with accum weight smaller than (1-confidence)
    |> (*) -1.                        // in any case, multiply it by -1 to get the VaR

// page 34
// assuming normality:
let var confidence mean stdDev =
    -MathNet.Numerics.Distributions.Normal(mean, stdDev).InverseCumulativeDistribution(1.-confidence)

// assuming normality:
let stdDevFromVaR varsConfidence var =
    let normal = MathNet.Numerics.Distributions.Normal()
    - var/normal.InverseCumulativeDistribution(1.-varsConfidence)

// page 34
// assuming normality:
let expectedShortfall confidence stdDev =
    let p = 1. - confidence
    let normal = MathNet.Numerics.Distributions.Normal()
    stdDev * normal.Density(normal.InverseCumulativeDistribution(p)) / p

let monthlyReturns (series:Series<DateTime,_>) =
    let aggregator = Aggregation.ChunkWhile<DateTime>(fun a b -> a.Year = b.Year && a.Month = b.Month)
    let keyGen (s:DataSegment<Series<DateTime,float>>) =
        let firstDayOfMonth (date:DateTime) = DateTime(date.Year, date.Month, 1)
        firstDayOfMonth (s.Data.FirstKey())
    let valueGen (s:DataSegment<Series<DateTime,float>>) =
        Seq.sum s.Data.Values
        |> Some
        |> OptionalValue.ofOption

    Series.aggregateInto aggregator keyGen valueGen series

let empiricalExpectedShortfall var (data:Series<DateTime,_>) =
    data
    |> Series.filter (fun _ ret -> ret < -var) // returns worse than -var
    |> Series.values
    |> Seq.average                             // - expected shortfall
    |> (*) -1.                                 // expected shortfall

// Exercises from the book Elements of Financial Risk Management, 2nd Edition by Peter F. Christoffersen. Published by AP
// The data and solutions for the exercises are available at: http://booksite.elsevier.com/9780123744487/
// Chapter 1 - To be done
// Chapter 2 - To be done
// Chapter 3 - To be done
// Chapter 4 - To be done
// Chapter 5 - To be done
// Chapter 6 - To be done
// Chapter 7 - To be done
// Chapter 8 - To be done
// Chapter 9 - To be done
// Chapter 10 - To be done
// Chapter 11 - To be done
// Chapter 12 - To be done
// Chapter 13 - To be done


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

[<EntryPoint>]
let main argv =
    workshop2 ()
    0