module Program

open Elmish
open Elmish.UrlParser
open Fable.Core.JsInterop

importAll "./sass/app.scss"

open Elmish.React
open Elmish.Debug
open Elmish.HMR

open App.State

// App
Program.mkProgram init update (fun model dispatch -> App.View.root { model = model; dispatch = dispatch })
|> Program.toNavigable (parseHash Router.route) urlUpdate
(*
#if DEBUG
|> Program.withDebugger
#endif
*)
|> Program.withReactBatched "elmish-app"
|> Program.run
