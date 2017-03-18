namespace RiskManagement

module Workshop6 =
    open RiskManagement
    open Core
    open Utils
    open NonnormalDistributions
    open Asymmetric_t_Distribution
    open Microsoft.FSharp.Collections
    open MathNet.Numerics
    open NUnit.Framework
    open FsUnit

    let ``workshop 6.1``() =
        let chi = Distributions.ChiSquared(4.0)

        [for _ in 1..2513 -> chi.Sample()]
        |> qqPlot

    [<Test>]
    let ``workshop 6.5``() =
        let objectiveD1, objectiveD2 =
            10.1411553, -0.4785165
        (*
        let ds,sse = ``solve for d1 & d2 given`` -1. 2.
        let d1, d2 = ds.["d1"], ds.["d2"]
        d1
        |> should be (equalWithin 0.0001 objectiveD1)

        d2
        |> should be (equalWithin 0.0001 objectiveD2)
        *)

        let ζ1, ζ2 = ``ζ1, ζ2`` objectiveD1 objectiveD2
        ζ1
        |> should be (equalWithin 0.001 -1.)
        ζ2
        |> should be (equalWithin 0.001 2.)
    
    let manualTests() =
        ``workshop 6.1``()