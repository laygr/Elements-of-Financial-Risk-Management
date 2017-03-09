namespace RiskManagement

module Workshop7 =
    open RiskManagement
    open Core
    open Utils
    open NonnormalDistributions
    open CovarianceAndCorrelationModels
    open Microsoft.FSharp.Collections

    // Workshop 7-3
    let ``workshop 7-3`` vimexReturns ipcReturns bondReturns initialVariance lambda =
        
        let rec varianceRM returns =
            seq {
                yield initialVariance
                yield!
                    Seq.zip (varianceRM returns) returns
                    |> Seq.map (fun (var, ret) -> riskMetrics var (ret**2.) lambda)
            }

        let vimexVarRM, ipcVarRM, bondVarRM =
            varianceRM vimexReturns,
            varianceRM ipcReturns,
            varianceRM bondReturns

        let vimexZ, ipcZ, bondZ =
            standarizeReturns vimexReturns vimexVarRM,
            standarizeReturns ipcReturns ipcVarRM,
            standarizeReturns bondReturns bondVarRM

        let ``ln(CLc)`` lambda =
            let qRM = qRM lambda

            let qRMVimexVimex, qRMIPCIPC, qRMBondBond, qRMVimexIPC, qRMVimexBond, qRMIPCBond =
                qRM vimexZ vimexZ 1.,
                qRM ipcZ   ipcZ   1.,
                qRM bondZ  bondZ  1.,
                qRM vimexZ ipcZ  (correlationTargeting vimexZ ipcZ),
                qRM vimexZ bondZ (correlationTargeting vimexZ bondZ),
                qRM ipcZ   bondZ (correlationTargeting ipcZ bondZ)

            let vimexIPCRho, vimexBondRho, ipcBondRho =
                rho qRMVimexIPC qRMVimexVimex qRMIPCIPC,
                rho qRMVimexBond qRMVimexVimex qRMBondBond,
                rho qRMIPCBond qRMIPCIPC qRMBondBond

            let zs = [|vimexZ; ipcZ; bondZ|]
            let rhos = Array2D.create 3 3 Seq.empty
            rhos.[0,1] <- vimexIPCRho
            rhos.[0,2] <- vimexBondRho
            rhos.[1,2] <- ipcBondRho

            CovarianceAndCorrelationModels.``ln(CLc)`` zs rhos
        0.0 // ToDo: maximize ``ln(CLc)``