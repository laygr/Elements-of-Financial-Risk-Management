namespace RiskManagement
open Core
open Utils

// Chapter 7: Covariance and Correlation Models
module CovarianceAndCorrelationModels =

    // P. 160
    // Correlation dynamics by RiskMetrics
    let rec qRM lambda zA zB initialQ =
        seq {
            yield initialQ
            yield!
                Seq.zip3 (qRM lambda zA zB initialQ) zA zB
                |> Seq.map (fun (q, zA, zB) -> riskMetrics q (zA*zB) lambda)
        }

    // P. 161
    let correlationTargeting zA zB =
        (sumProduct zA zB) / (float <| Seq.length zA)

    let rho (qab:float seq) (qa:float seq) (qb:float seq) =
        Seq.zip3 qab qa qb
        |> Seq.map (fun (qab, qa, qb) -> qab/(qa*qb))

    (*P. 163.
    The inner part that can be reutilized in, for example, 3.4, P. 165
    *)
    let ``ln(CLc)'`` (zA, zB, rhoAB) =
        log(1. - rhoAB**2.) + (zA**2. + zB**2. - 2. * rhoAB * zA * zB)/(1. - rhoAB**2.)

    // P. 165
    let ``ln(CLc)`` (zs:float seq [] ) (rhos:float seq [,]) =
        let T = Seq.length zs.[0]
        let n = Array.length zs
        
        -1./2. * (
                [for i in 0 .. n-1 do
                 for j in (i+1) .. n-1 do
                    yield
                        Seq.zip3 zs.[i] zs.[j] rhos.[i,j]
                        |> Seq.map ``ln(CLc)'``
                        |> Seq.sum
                ] |> Seq.sum
            )