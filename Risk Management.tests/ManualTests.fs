namespace RiskManagement.Tests

// Tests that need to be run as console applications
module ManualTests =

    open RiskManagement
    open RiskManagement.Tests.NonnormalDistributions
    open RiskManagement.NonnormalDistributions
    open Deedle
    open FsUnit
    open NUnit.Framework
    open MathNet.Numerics.Distributions

    (*
    [<Test>]
    let ``Workshop 4``() =
        let frame = Frame.ReadCsv("C:\Users\laygr\Desktop\Risk Management\Risk Management\Risk Management.tests\Workshop4 data.csv")
        let solution, value = Workshop4.w4_1 frame
        solution
        |> Seq.iter (fun d -> printfn "%s = %f" d.Key d.Value)
        printfn "sum of loglikelihoods = %f" value
        Assert.AreEqual(1, 1)
    *)

    
    
    [<EntryPoint>]
    let main args =
        Workshop6.manualTests()
        //manualTestsForNonnormalDistributions()
        0