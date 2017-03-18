namespace RiskManagement
module TimeSeries =
    open MathNet.Numerics.LinearRegression
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.Distributions
    open System
    open RMStatistics
    open Deedle

    type Frequency =
        | EveryNMilliSeconds of float
        | EveryMilliSecond
        | EveryNSeconds of float
        | EverySecond
        | EveryNMinutes of float
        | EveryMinute
        | EveryNHours of float
        | EveryHour
        | EveryNDays of float
        | Daily
        | Weekly
        | Monthly
        | Yearly

    type Frequency with
        member this.TimeSpan =
            match this with
            | EveryNMilliSeconds n -> TimeSpan.FromMilliseconds n
            | EveryMilliSecond -> TimeSpan.FromMilliseconds 1.
            | EveryNSeconds n -> TimeSpan.FromSeconds n
            | EverySecond -> TimeSpan.FromSeconds 1.
            | EveryNMinutes n -> TimeSpan.FromMinutes n
            | EveryMinute -> TimeSpan.FromMinutes 1.
            | EveryNHours n -> TimeSpan.FromHours n
            | EveryHour -> TimeSpan.FromHours 1.
            | EveryNDays n -> TimeSpan.FromDays n
            | Daily -> TimeSpan.FromDays 1.
            | Weekly -> TimeSpan.FromDays 7.
            | Monthly -> raise <| Exception("Months have not a unique TimeSpan")
            | Yearly -> raise <| Exception("Years have not a unique TimeSpan")

    let ``t+1`` (t:DateTime) =
        function
        | EveryNMilliSeconds n -> t + TimeSpan.FromMilliseconds(n)
        | EveryMilliSecond -> t + TimeSpan.FromMilliseconds(1.)
        | EveryNSeconds n -> t + TimeSpan.FromSeconds(n)
        | EverySecond -> t + TimeSpan.FromSeconds(1.)
        | EveryNMinutes n -> t + TimeSpan.FromMinutes(n)
        | EveryMinute -> t + TimeSpan.FromMinutes(1.)
        | EveryNHours n -> t + TimeSpan.FromHours(n)
        | EveryHour -> t + TimeSpan.FromHours(1.)
        | EveryNDays n -> t + TimeSpan.FromDays(n)
        | Daily -> t + TimeSpan.FromDays(1.)
        | Weekly -> t + TimeSpan.FromDays(7.)
        | Monthly ->
            let newDate' = t.AddMonths(1)
            DateTime(newDate'.Year, newDate'.Month, 1)
        | Yearly -> DateTime(t.Year+1, 1, 1)
        
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

    let autocorrelationSignificanceForLag confidence (values:float[]) lag slope0 =
        let testSlope confidence (slope:float) slopeStandardError (sampleSize:int) slope0 =
            let degreesOfFreedom = float <| sampleSize - 2
            let t = RMStudentT(0.,0., degreesOfFreedom)
            hypothesisTest t slope slope0 slopeStandardError

        let slope,_,slopeStandardError = autocorrelationWithLag values lag
        testSlope confidence slope slopeStandardError (values.Length) slope0

    // tests if the jointly autocorrelation is zero up to "autocorrelations.Length" lags
    // autocorrelations from smaller lag to larger lag
    let ljung_boxTest confidence (autocorrelations:float[]) =
        let nautocorrelations = float autocorrelations.Length
        let autocorrelations_index = Array.zip autocorrelations [|1. .. nautocorrelations|]
        let lb = nautocorrelations * (nautocorrelations + 2.) + Seq.fold (fun acum (autocorrelation,i)-> acum + (autocorrelation**2./(nautocorrelations - i))) 0. autocorrelations_index

        let chisquared = new ChiSquared(nautocorrelations)
        let criticalValue = chisquared.InverseCumulativeDistribution(confidence)
        lb <= criticalValue
    
    let ``garch(1, 1)`` nextGenerator initialValue knownValues =
        let rec sequence =
            seq {
                yield initialValue
                yield!
                    Seq.zip sequence knownValues
                    |> Seq.map nextGenerator
            }
        sequence

    // P. 12
    let riskMetrics lambda initialValue knownValues =
        let riskMetrics' (withL, withL') =
            lambda * withL + (1. - lambda) * withL'

        let rec sequence =
            seq {
                yield initialValue
                yield!
                    Seq.zip sequence knownValues
                    |> Seq.map riskMetrics'
            }
        sequence

    type TimeSeries(frequency, times', values':float seq) =
        inherit Series<DateTime, float>(times', values')

        static member fromSeries frequency series =
            let times = Series.keys series
            let values = Series.values series
            TimeSeries(frequency, times, values)

        member this.Times = this.Keys

        member this.Returns method position =
            let returns = Core.returnsForPrices position method this.Values
            TimeSeries(frequency, this.Times, returns)
            |> Series.shift 1

        member this.AggregateIntoByMonth =
            let aggregator = Aggregation.ChunkWhile<DateTime>(fun a b -> a.Year = b.Year && a.Month = b.Month)
            let keyGen (s:DataSegment<Series<DateTime,float>>) =
                let firstDayOfMonth (date:DateTime) = DateTime(date.Year, date.Month, 1)
                firstDayOfMonth (s.Data.FirstKey())
            let valueGen (s:DataSegment<Series<DateTime,float>>) =
                Seq.sum s.Data.Values
                |> Some
                |> OptionalValue.ofOption

            Series.aggregateInto aggregator keyGen valueGen this
            |> TimeSeries.fromSeries Monthly

        member this.AggregateByYear =
            let aggregator = Aggregation.ChunkWhile<DateTime>(fun a b -> a.Year = b.Year)
            let keyGen (s:DataSegment<Series<DateTime,float>>) =
                let firstDayOfYear (date:DateTime) = DateTime(date.Year, 1, 1)
                firstDayOfYear (s.Data.FirstKey())

            Series.aggregate aggregator keyGen this
            |> Series.mapValues (fun segment -> TimeSeries.fromSeries frequency segment.Data)

        member this.AggregateByFrequency (frequency:Frequency) =
            let aggregator =
                let size =
                    frequency.TimeSpan.TotalMilliseconds / frequency.TimeSpan.TotalMilliseconds
                    |> int
                if size = 0 then raise <| Exception("It's only supported grouping to lower frequencies")
                Aggregation.ChunkSize (size, Boundary.AtEnding)
            let keySelector (s:DataSegment<Series<DateTime,float>>) = s.Data.FirstKey()

            Series.aggregate aggregator keySelector this
            |> Series.mapValues (fun segment -> TimeSeries.fromSeries frequency segment.Data)
        
        member this.RiskMetrics lambda initialValue =
            let results = riskMetrics lambda initialValue this.Values
            let times = Series.keys this
            let lastTime = Seq.last times
            let newKeys = Seq.append times [``t+1`` lastTime frequency]
            
            TimeSeries(frequency, newKeys, results)