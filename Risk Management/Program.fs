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

let fr:Frame<DateTime,string> =
    Frame.ReadCsv("C:\Users\laygr\Desktop\data.csv")
    |> Frame.indexRowsUsing toDateTime

let returnWhenLonging (frame:Frame<DateTime,string>) =
    log frame?Close - log (Frame.shift 1 frame)?Close

let returnWhenShorting frame =
    log (Frame.shift 1 frame)?Close - log frame?Close

fr?Return <- returnWhenShorting fr

let RiskMetrics lambda (frame:Frame<DateTime,string>) =
    let v = Stats.variance frame?Return
    let next pastVariance' pastReturn' =
        match pastVariance', pastReturn' with
        | Some pastVariance, Some pastReturn -> Some (lambda * pastVariance + (1.-lambda) * pastReturn)
        | _ -> pastVariance'
    Frame.shift 1 frame
    |> Frame.getCol("Return")
    |> Series.scanAllValues next (Some v)

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

let hs confidence (days:int) (data:Frame<DateTime,string>) (day:DateTime) =
    (nRowsBeforeKey days data day)?Return
    |> Series.values
    |> fun v -> Statistics.quantileCustomFunc v QuantileDefinition.Excel
    |> (|>) confidence
    |> (*) -1.

let whs confidence eta (days:int) (data:Frame<DateTime,_>) (day:DateTime) =
    let subframe = nRowsBeforeKey days (data.Clone()) day

    let taus = Series.scanValues (fun counter next -> counter - 1) (days+1) (subframe.GetColumnAt(0))
    subframe?Tau <- taus
    
    let weight tau =
      eta**(float(tau - 1)) * (1.-eta)/(1.-eta ** (float days))

    let toWeight (os:ObjectSeries<_>) =
        let tau = (os.Get "Tau") :?> int
        weight tau

    subframe?Weight <- Frame.mapRowValues toWeight subframe

    let sorted = subframe.SortRowsBy("Return",fun i -> i)
    sorted?AcumWeight <- Series.scanValues (fun acum w -> acum + w) 0. sorted?Weight

    let filtered = 
        sorted
        |> Frame.filterRows (fun _ row -> row?AcumWeight < confidence)

    if filtered.RowCount = 0
    then
        sorted
        |> Frame.getCol("Return")
        |> Series.firstValue
    else
        filtered
        |> Frame.getCol("Return")
        |> Series.lastValue
    |> (*) -1.

let monthlyReturns (frame:Frame<DateTime,_>) =
    let aggregator = Aggregation.ChunkWhile<DateTime>(fun a b -> a.Year = b.Year && a.Month = b.Month)
    let keyGen (s:DataSegment<Series<DateTime,float>>) =
        let firstDayOfMonth (date:DateTime) = DateTime(date.Year, date.Month, 1)
        firstDayOfMonth (s.Data.FirstKey())
    let valueGen (s:DataSegment<Series<DateTime,float>>) =
        Seq.sum s.Data.Values
        |> Some
        |> OptionalValue.ofOption

    Series.aggregateInto aggregator keyGen valueGen (frame?Return)

[<EntryPoint>]
let main argv =
    let day = DateTime(1987,10,1)
    let rows = Frame.takeLast 22 fr
    fr?HS <- mapKey (hs 0.99 250 fr) rows
    fr?WHS <- mapKey (whs 0.01 0.99 250 fr) rows
    fr?RiskMetrics <- RiskMetrics 0.94 fr
    fr?MonthlyReturn <- monthlyReturns fr
    fr.SaveCsv("riskmetricstest.csv")
    0