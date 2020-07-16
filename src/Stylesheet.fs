[<RequireQualifiedAccess>]
module Stylesheet

open Fable.Core.JsInterop

let inline apply (relativePath: string) : unit = importSideEffects relativePath