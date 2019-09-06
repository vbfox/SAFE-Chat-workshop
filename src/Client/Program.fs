module Program

open Elmish
open Elmish.UrlParser
open Fable.Core.JsInterop

importSideEffects  "./sass/app.scss"
importSideEffects  "bootstrap/dist/css/bootstrap.min.css"

open Elmish.React
open Elmish.Debug
open Elmish.HMR

open App.State

// App
Program.mkProgram init update App.View.root
|> Program.toNavigable (parseHash Router.route) urlUpdate
//#if DEBUG
//|> Program.withDebugger
//#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
