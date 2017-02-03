module RiskM

open Deedle
open System
open MathNet.Numerics.LinearRegression
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
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

// index for row with key
// index starting at 0
let indexForKey (frame:Frame<'K,_>) (key:'K) =
    frame.RowIndex.Locate key
    |> frame.RowIndex.AddressOperations.OffsetOf

// position of row with key.
// Index beginning with 1
let positionForKey (frame:Frame<'K,_>) (key:'K) =
    indexForKey frame key
    |> (+) (int64 1)

let nRowsBeforeKey rows (data:Frame<_,_>) key =
    data
    |> Frame.take (int (indexForKey data key))
    |> Frame.takeLast rows

let mapKey (f:'a -> 'b) (frame:Frame<_,_>) =
    Series.map (fun k _ -> f k) (frame.GetColumnAt 0)

let historicalSimulation confidence =
    Series.values
    >> fun v -> Statistics.quantileCustomFunc v QuantileDefinition.Excel
    >> (|>) (1. - confidence)
    >> (*) -1.

let weightedHistoricalSimulation confidence eta (data':Frame<DateTime,_>) =
    let data = data'.Clone()
    let days = data.RowCount
    let taus = Series.scanValues (fun counter next -> counter - 1) (days+1) (data.GetColumnAt(0))
    data?Tau <- taus
    
    let weight tau =
      eta**(float(tau - 1)) * (1.-eta)/(1.-eta ** (float days))

    let toWeight (os:ObjectSeries<_>) = // giver a row, it returns its corresponding weight
        let tau = (os.Get "Tau") :?> int
        weight tau

    data?Weight <- Frame.mapRowValues toWeight data // adds a column with the weigths

    let sorted = data.SortRows("Return")                                                // sorted by returns
    sorted?AccumWeight <- Series.scanValues (fun accum w -> accum + w) 0. sorted?Weight // create column of accumulated weights
                                                                // days with accum weight smaller than (1-confidence)
    sorted
    |> Frame.filterRows (fun _ row -> row?AccumWeight >= (1. - confidence))
    |> Frame.getCol("Return")
    |> Series.firstValue          // return the first day of the days with accum weight equal or larger than (1-confidence)
    |> (*) -1.                    // multiply it by -1 to get the VaR

// page 34
// Value at risk
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

let empiricalExpectedShortfall var =
    Series.filter (fun _ ret -> ret < -var) // returns worse than -var
    >> Series.values
    >> Seq.average                          // - expected shortfall
    >> (*) -1.                              // expected shortfall

let autocorrelationWithLag (values:float[]) lag =
    let valuesLength  = values.Length
    let beforeValues = Array.init (lag + valuesLength) (fun i -> if i < lag then 0. else values.[i-lag])
    let afterValues = Array.init (lag + valuesLength) (fun i -> if i >= valuesLength then 0. else values.[i])
    let (slope,intercept) = SimpleRegression.Fit(beforeValues,afterValues)
    let errors = Array.init (lag + valuesLength ) (fun i -> afterValues.[i] - intercept - slope * beforeValues.[i])
    let errorsVariance = Statistics.Variance errors
    let ssx = Array.fold (fun acum x -> acum + x**2.) 0. beforeValues
    let slopeStandardError = Math.Sqrt(errorsVariance/ssx)
    slope,intercept,slopeStandardError

let testSlope confidence (slope:float) slopeStandardError (sampleSize:int) slope0 =
    let degreesOfFreedom = float <| sampleSize - 2
    let t = StudentT(0., 0., degreesOfFreedom)
    let rightTailProbability = 1. - (1. - confidence)/2.
    let criticalValue = t.InverseCumulativeDistribution(rightTailProbability)
    let tStatistic = (slope - slope0)/slopeStandardError
    Math.Abs(tStatistic) < criticalValue

let autocorrelationSignificanceForLag confidence (values:float[]) lag slope0 =
    let slope,_,slopeStandardError = autocorrelationWithLag values lag
    testSlope confidence slope slopeStandardError (values.Length) slope0


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

[<EntryPoint>]
let main argv =
    workshop2 ()
    0