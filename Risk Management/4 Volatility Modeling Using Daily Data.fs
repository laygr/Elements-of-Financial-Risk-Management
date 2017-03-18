namespace RiskManagement
module VolatilityModelingUsingDailyData =
    open System
    open Accord.Math.Optimization
    open System.Collections.Generic
    open Core

    // P. 70
    let ``variances from garch(1, 1)`` alpha beta omega returns initialVariance =
        let ``variance t`` (``variance t-1``, ``return t-1``) =
                omega + beta * ``variance t-1`` + alpha * ``return t-1``**2.

        TimeSeries.``garch(1, 1)`` ``variance t`` initialVariance returns

    let ``maximize garch(1, 1)`` returns initialVariance =
        let alphaIndex, betaIndex, omegaIndex = 0, 1, 2

        let sumOfLogLikelihoods (vars:float[]) =

            let alpha, beta, omega = vars.[alphaIndex], vars.[betaIndex], vars.[omegaIndex]
            let variances = ``variances from garch(1, 1)`` alpha beta omega returns initialVariance

            let logLikelihood (var, ret) =
                -1. / 2. * (log var + ret**2. / var)
                
            Seq.zip variances returns
            |> Seq.map logLikelihood
            |> Seq.sum

        let persistence (vars:float[]) =
            vars.[alphaIndex] + vars.[betaIndex]

        let var index (vars:float[]) =
            vars.[index]

        let obj = NonlinearObjectiveFunction(3, sumOfLogLikelihoods)
        let constraints = List<NonlinearConstraint>()
        constraints.Add(NonlinearConstraint(obj, persistence, ConstraintType.LesserThanOrEqualTo, 0.99999))
        constraints.Add(NonlinearConstraint(obj, (var 0), ConstraintType.GreaterThanOrEqualTo, 0.0))
        constraints.Add(NonlinearConstraint(obj, (var 1), ConstraintType.GreaterThanOrEqualTo, 0.0))
        constraints.Add(NonlinearConstraint(obj, (var 2), ConstraintType.GreaterThanOrEqualTo, 0.0))
        let solver = Cobyla(obj, constraints)
        
        let success = solver.Maximize()
        let loglikelihood = solver.Value
        let solution = solver.Solution
        let status = solver.Status
        dict [
            "alpha", solution.[0]
            "beta", solution.[1]
            "omega", solution.[2]
        ], loglikelihood

    let ``already optimized variances from garch(1, 1)`` returns initialVariance =
        let parameters,_ = ``maximize garch(1, 1)`` returns initialVariance
        let alpha, beta, omega = parameters.["alpha"], parameters.["beta"], parameters.["omega"]

        ``variances from garch(1, 1)`` alpha beta omega returns initialVariance