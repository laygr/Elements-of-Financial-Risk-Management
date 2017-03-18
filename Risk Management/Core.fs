namespace RiskManagement
module RMStatistics =
    type IRMContinuousDistribution =
        abstract member InverseCumulativeDistribution : float -> float

    type RMStudentT(location, scale, degreesOfFreedom) =
        let distribution = MathNet.Numerics.Distributions.StudentT(location, scale, degreesOfFreedom)

        interface IRMContinuousDistribution with
            member this.InverseCumulativeDistribution probability =
                distribution.InverseCumulativeDistribution probability

    type HypothesisTestResult = | AcceptNullHypothesis | RejectNullHypothesis

    let hypothesisTest (distribution:IRMContinuousDistribution) realValue nullHypothesisValue confidence standardError =
        let rightTailProbability = 1. - (1. - confidence)/2.
        let leftTailProbability = confidence/2.
        let rightCriticalValue = distribution.InverseCumulativeDistribution rightTailProbability
        let leftCriticalValue = distribution.InverseCumulativeDistribution leftTailProbability
        let tStatistic = (realValue - nullHypothesisValue)/standardError

        if tStatistic >= leftCriticalValue && tStatistic <= rightCriticalValue
        then AcceptNullHypothesis
        else RejectNullHypothesis

module Core =

    open Deedle
    open System
    open MathNet.Numerics.LinearRegression
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.Distributions
    open RProvider
    open RDotNet
    open RProvider.stats
    open RProvider.fArma

    type VaRMethod =
        | HistoricalSimulation
        | WeightedHistoricalSimulation
        | Statistical

    type ReturnKind = | Logarithmic | Arithmetic

    type Position = | Long | Short
    
    type Returns = {
        Sequence : float seq
        Kind : ReturnKind
    }

    type VaR = {
        CoverageRate : float // p. 22
        VaR : float
        Method : VaRMethod
    }

    type ES = {
        VaR : VaR
        ES : float
    }

    let excelPercentile values percentile =
        let sorted = Seq.sort values
        (Statistics.quantileCustomFunc sorted QuantileDefinition.Excel) percentile

    let returnsForPrices position method prices =
        let multiplier = if position = Long then 1. else -1.
        let f (``t``, ``t+1``) =
            multiplier *
                if method = Logarithmic
                then log ``t+1`` - log ``t``
                else (``t+1``-``t``)/``t``
        Seq.pairwise prices
        |> Seq.map f

    // p. 8
    let changeReturnsKind newKind returns =
        if newKind = returns.Kind
        then returns
        else
            let newSequence =
                returns.Sequence
                |> match newKind with
                   | Arithmetic -> Seq.map (fun ret -> (Math.Exp ret) - 1.)
                   | Logarithmic -> Seq.map (fun ret -> Math.Log (ret + 1.))
            
            { Sequence = newSequence; Kind = newKind }
            
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

        let toWeight (os:ObjectSeries<_>) = // given a row, it returns its corresponding weight
            let tau = (os.Get "Tau") :?> int
            weight tau

        data?Weight <- Frame.mapRowValues toWeight data // adds a column with the weigths

        let sorted = data.SortRows("Return")                          // sorted by returns
        sorted?AccumWeight <- Series.scanValues (+) 0. sorted?Weight  // create column of accumulated weights
                                                                      
        sorted
        |> Frame.filterRows (fun _ row -> row?AccumWeight >= (1. - confidence))  // days with accum weight smaller than (1-confidence)
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
        
    let empiricalExpectedShortfall var =
        Series.filter (fun _ ret -> ret < -var) // returns worse than -var
        >> Series.values
        >> Seq.average                          // - expected shortfall
        >> (*) -1.                              // expected shortfall

    // --- Chapter 3 --- //
    
    let arma p q (data:float list) =
        R.eval(R.parse(text="library(fArma)")) |> ignore
        let dataset =
            namedParams [ "xx", data ]
            |>R.data_frame
        let s4 = R.armaFit(R.as_formula("xx ~ arma(2,2)"), dataset).AsS4()
        s4.["fit"].AsList().["coef"].AsNumeric().ToArray()