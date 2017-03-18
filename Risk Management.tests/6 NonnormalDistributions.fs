namespace RiskManagement.Tests

module NonnormalDistributions =
    open RiskManagement.NonnormalDistributions
    open NUnit.Framework
    open FsUnit
    open MathNet.Numerics.Distributions
    
    let testQQPlot () =
        let student = StudentT(0.0, 1.0, 10.0)
        [for _ in 1 .. 100 -> student.Sample()] // returns that distribute as a student distribution
        |> qqPlot

    let manualTestsForNonnormalDistributions() =
        testQQPlot()

