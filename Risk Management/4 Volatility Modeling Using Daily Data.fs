namespace RiskManagement
module VolatilityModelingUsingDailyData =
    open System
    open Accord.Math.Optimization
    open System.Collections.Generic

    // P. 70
    let ``garch(1, 1)`` returns initialVariance =
        let alphaIndex = 0
        let betaIndex = 1
        let omegaIndex = 2

        let sumOfLogLikelihoods (returns':float[]) (vars:float[]) =
           let returns = Seq.ofArray returns'

           let alpha, beta, omega = vars.[alphaIndex], vars.[betaIndex], vars.[omegaIndex]

           let ``variance t`` (``variance t-1``, ``return t-1``) =
                omega + beta * ``variance t-1`` + alpha * ``return t-1``**2.

           let rec variances =
               seq {
                    yield initialVariance
                    yield!
                        Seq.zip variances returns
                        |> Seq.map ``variance t``
               }

           let logLikelihood (var, ret) =
                -1. / 2. * (log var + ret**2. / var)

           let logLikelihoods =
                Seq.zip variances returns
                |> Seq.map logLikelihood

           Seq.sum (Seq.take 314 logLikelihoods)

        let persistence (vars:float[]) =
            vars.[alphaIndex] + vars.[betaIndex]

        let var index (vars:float[]) =
            vars.[index]

        let obj = NonlinearObjectiveFunction(3, (sumOfLogLikelihoods returns))
        let constraints = List<NonlinearConstraint>()
        constraints.Add(NonlinearConstraint(obj, persistence, ConstraintType.LesserThanOrEqualTo, 0.99999))
        constraints.Add(NonlinearConstraint(obj, (var 0), ConstraintType.GreaterThanOrEqualTo, 0.0))
        constraints.Add(NonlinearConstraint(obj, (var 1), ConstraintType.GreaterThanOrEqualTo, 0.0))
        constraints.Add(NonlinearConstraint(obj, (var 2), ConstraintType.GreaterThanOrEqualTo, 0.0))
        let solver = Cobyla(obj, constraints)
        
        let success = solver.Maximize()
        let value = solver.Value
        let solution = solver.Solution
        let status = solver.Status
        dict [
            "alpha", solution.[0]
            "beta", solution.[1]
            "omega", solution.[2]
        ], value