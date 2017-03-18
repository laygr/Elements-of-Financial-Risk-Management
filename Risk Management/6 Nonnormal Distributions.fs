namespace RiskManagement

module NonnormalDistributions =
    open MathNet.Numerics
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.Distributions
    open Utils
    open FSharp.Charting
    open System
    open FSharp.Charting
    open Core

    // P. 124
    let qqPlot (returns:float seq) =
        let mean = Statistics.Mean returns
        let stdDev = Statistics.StandardDeviation returns
        let sorted = Seq.sort returns
        let n = Seq.length returns |> float

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
            |> Seq.map (fun x -> x, x*stdDev + mean)
            
        [Chart.Line(returnPoints, Name="returns", Color=Drawing.Color.RoyalBlue)
         Chart.Line(normalPoints, Name="normal", Color=Drawing.Color.Red)]
        |> Chart.Combine
        |> Chart.WithXAxis(Title="Normal quantile", Max=8.0, Min=(-8.0), MinorTickMark=ChartTypes.TickMark(Interval=1.0))
        |> Chart.WithYAxis(Title="Return quantile", Max=8.0, Min=(-8.0), MinorTickMark=ChartTypes.TickMark(Interval=1.0))
        |> Chart.WithTitle "QQ Plot"
        |> Chart.Show 

    // P. 125
    let standarizeReturns returns variances =
        Seq.zip returns variances
        |> Seq.map (fun (ret,var) -> ret/(sqrt var))

    // P. 124
    // gives a VaR for t+1 given t+1-m returns
    let ``VaR via filtered historical simulation`` returns variances coverageRate =
        let standarizedReturns = standarizeReturns returns variances
        let ``t+1 variance`` = Seq.last variances
        -``t+1 variance`` * excelPercentile standarizedReturns coverageRate

    let ``ES from FHS`` returns var : float =
        returns
        |> Seq.filter (fun r -> r < -var)
        |> Seq.average

    let ``FHS via Garch(1,1)`` returns initialVariance alpha beta omega coverageRate =
        let variances = VolatilityModelingUsingDailyData.``variances from garch(1, 1)`` alpha beta omega returns initialVariance
        ``VaR via filtered historical simulation`` returns variances coverageRate

    // p. 127
    let ``CF^-1_p`` returns coverageRate =
        let ζ1, ζ2 = Statistics.SkewnessKurtosis returns // skewness, kurtosis
        let normal = Normal()
        let n = normal.InverseCumulativeDistribution(coverageRate)

        n
        + ζ1/6.0 * (n**2.0 - 1.0)
        + ζ2/24.0 * (n**3.0 - 3.0*n)
        - ζ1**2.0/36.0 * (2.0*n**3.0 - 5.0*n)

    let ``VaR via Corner-Fisher approximation to VaR`` (returns:float seq) coverageRate =
        let stdDev = Statistics.StandardDeviation returns

        -stdDev * (``CF^-1_p`` returns coverageRate)
    
    let ``ES from CF`` (returns:float seq) var : float =
        let stdDev = Statistics.StandardDeviation returns
        let ``ES_{CF(p)}`` =
            let ζ1, ζ2 = Statistics.SkewnessKurtosis returns // skewness, kurtosis
            let normal = Normal()
            let ``CF^-1_p`` = ``CF^-1_p`` returns var.CoverageRate
            let n = normal.CumulativeDistribution ``CF^-1_p``
            
            n/var.CoverageRate
            * (1.0 + ζ1/6.0*``CF^-1_p``**3.0 + ζ2/24.0*(``CF^-1_p``**4.0 - 2.0*``CF^-1_p``**2.0 - 1.0))
        - stdDev * ``ES_{CF(p)}``

    // p. 128
    module Standarized_t_Distribution =
        open Accord.Math.Optimization
        open System.Collections.Generic

        let Γ z = SpecialFunctions.Gamma z
        // p. 128, 131
        let d ζ2 =
            6.0/ζ2 + 4.0
        
        let z d x =
            x/(sqrt (d/(d-2.0)))

        let C d =
            (Γ ((d + 1.0)/2.0)) / ((Γ (d/2.0)) * sqrt (Math.PI * (d-2.0)))

        // for d > 2
        let ``f_{\tile{t}(d)}`` d z = // \tile{t}(d)

            (C d) * (1.0 + (z ** 2.0)/(d - 2.0))**(-(1.0+d)/2.0)
            
        // p. 129
        let ``maximize f_{\tile{t}(d)} optimizing d`` returns =
            let dIndex = 0

            let lnL_1 (vars:float[]) =

               let d = vars.[dIndex]
               
               Seq.map (z d >> ``f_{\tile{t}(d)}`` d) returns // densities
               |> Seq.map log
               |> Seq.sum

            let obj = NonlinearObjectiveFunction(1, lnL_1)
            let constraints = List<NonlinearConstraint>()
            let solver = Cobyla(obj, constraints)
        
            let success = solver.Maximize()
            let loglikelihood = solver.Value
            let solution = solver.Solution
            let status = solver.Status
            dict [
                "d", solution.[0]
            ], loglikelihood

        // p. 131
        let ``t^{-1}`` d coverageRate =
            let tDistribution = StudentT(0.0, 1.0, d)
            tDistribution.InverseCumulativeDistribution(coverageRate)

        let ``\tile{t}^{-1}`` d coverageRate =
            sqrt ((d-2.0)/d) * (``t^{-1}`` d coverageRate)

        let VaR stdDev d coverageRate =
            let var = (-stdDev) * (``\tile{t}^{-1}`` d coverageRate)
            {
                CoverageRate = coverageRate
                VaR = var
                Method = Statistical
            }

        let ``ES^p`` stdDev d VaR =
            let ``ES_{\tile{t}(d)}(p)`` =
                let p = VaR.CoverageRate
                (C d)/p * ((1.0 + 1.0/(d-2.0)*(``\tile{t}^{-1}`` d p)**2.0)**((1.0-d)/2.0) * (d-2.0)/(1.0-d))
            (-stdDev) * ``ES_{\tile{t}(d)}(p)``

        
    // p. 133
    module Asymmetric_t_Distribution =
        open Accord.Math.Optimization
        open System.Collections.Generic
        open Standarized_t_Distribution

        // p. 133
        let ``A, B, C`` d1 d2 =
            let C = C d1
            let A = 4.*d2*C*(d1-2.)/(d1-1.)
            
            A
            , sqrt (1.0 + 3.0 * (d2**2.0) - A**2.0)
            , C

        // p. 133
        // for d1 > 2; -1 < d2 < 1
        let ``f_{asyt}`` (d1:float) (d2:float) z =
            let A, B, C = ``A, B, C`` d1 d2
            
            if z < (-A/B)
            then
                B*C*(1.0 + (B*z + A)*2.0)/((1.-d2)**2.0 * (d1 - 2.0))**((-1.0+d1)/2.0)
            else
                B*C*(1.0 + (B*z + A)*2.0)/((1.+d2)**2.0 * (d1 - 2.0))**((-1.0+d1)/2.0)
        
        // p. 134
        // for m3: d1 > 3
        // for m4: d1 > 4
        let ``m2, m3, m4`` d1 d2 =
            let _, _, C = ``A, B, C`` d1 d2
            1. + 3. * d2**2.
            , 16. * C * d2 * (1. + d2**2.) * (d1-2.)**2./((d1-1.)*(d1-3.))
            , 3. * (d1 - 2.)/(d1 - 4.) * (1. + 10.*d2**2. + 5.*d2**4.)
            
        // p. 134
        let ``f_{asyt} estimation`` =
            0.
            // to be done

        // p. 136
        let ζ1 d1 d2 =
            let A, B, _ = ``A, B, C`` d1 d2
            let m2, m3, m4 = ``m2, m3, m4`` d1 d2
            (m3 - 3.*A*m2 + 2. * A**3.)/B**3.

        let ζ2 d1 d2 =
            let A, B, _ = ``A, B, C`` d1 d2
            let m2, m3, m4 = ``m2, m3, m4`` d1 d2
            (m4 - 4.*A*m3 + 6. * A**2.*m2 - 3. * A**4.)/(B**4. - 3.)

        let ``ζ1, ζ2`` d1 d2 =
            let A, B, _ = ``A, B, C`` d1 d2
            let m2, m3, m4 = ``m2, m3, m4`` d1 d2
            
            (m3 - 3.*A*m2 + 2. * A**3.)/B**3.
            , (m4 - 4.*A*m3 + 6. * A**2.*m2 - 3. * A**4.)/B**4. - 3.
            
        // p. 136
        let ``solve for d1 & d2 given`` ζ1Known ζ2Known =
        
            let SSE (vars:float[]) =
                let d1, d2 = vars.[0], vars.[1]
                let ζ1', ζ2' = ``ζ1, ζ2`` d1 d2
                (ζ1Known - ζ1')**2. + (ζ2Known - ζ2')**2.

            let ζ1 (vars:float[]) = ζ1 vars.[0] vars.[1]
            let ζ2 (vars:float[]) = ζ2 vars.[0] vars.[1]

            let var index (vars:float[]) =
                vars.[index]
            
            let obj = NonlinearObjectiveFunction(2, SSE)
            let constraints = List<NonlinearConstraint>()
            constraints.Add(NonlinearConstraint(obj, var 0, ConstraintType.GreaterThanOrEqualTo, 4.))

            let solver = Cobyla(obj, constraints)
            let success = solver.Minimize()
            let sse = solver.Value
            let solution = solver.Solution
            let status = solver.Status
            dict [
                "d1", solution.[0]
                "d2", solution.[1]
            ], sse

        // p. 136
        let ``F^{-1}_{asyt}`` p d1 d2 =
            let A, B, C = ``A, B, C`` d1 d2
            if p < (1. - d2)/2.
            then
                1./B * ((1. - d2) * sqrt((d1 - 2.)/d1) * (``t^{-1}`` d1 (p/(1.-d2))) - A)
            else
                1./B * ((1. + d2) * sqrt((d1 - 2.)/d1) * (``t^{-1}`` d1 ((p+d2)/(1.+d2))) - A)

        // p. 136
        let VaR stdDev d1 d2 coverageRate =
            let p = coverageRate
            let var = (-stdDev) * (``F^{-1}_{asyt}`` p d1 d2)
            {
                CoverageRate = p
                VaR = var
                Method = Statistical
            }

        // p. 136, 144
        let ES stdDev d1 d2 VaR =
            let p = VaR.CoverageRate
            let Q = ``F^{-1}_{asyt}`` p d1 d2
            let A, B, C = ``A, B, C`` d1 d2

            let ``ES_{asyt}`` =
                let td1 = StudentT(0.,1.,d1)
                let td1' = td1.CumulativeDistribution(sqrt(d1/(d1-2.))*((B*Q+A)/(1.-d2)))
                
                C*(1.-d2)**2./(B*p)
                * ((1.+(1./(d1-2.))*((B*Q+A)/(1.-d2))**2.)**((1.-d1)/2.) * (d1-2.)/(1.-d1))
                -
                (A*C*(1.-d2))/(B*p) * (sqrt(Math.PI *(d1-2.))*Γ(d1/2.))/(Γ((d1+1.)/2.))* td1'
                
            (-stdDev) * ``ES_{asyt}``