namespace RiskManagement.Workshops

module Common =
    open Deedle
    open RiskManagement.DeedleExtensions
    
    let loadFrameFromCsv (filePath:string) dateColumnName =
        Frame.ReadCsv(filePath)
        |> Frame.indexRowsByDateTime dateColumnName
        |> Frame.sortRowsByKey

