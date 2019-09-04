module App.View

open Fable.Websockets.Elmish.Types
open App.Types
open Router
open Connection.Types
open Fable.React
open Props

type RootProps = {
    model: App.Types.Model
    dispatch: App.Types.Msg -> unit
}

let private getDispatchChannelMessage = memoizeOnceTupled (fun (dispatch: App.Types.Msg -> unit, chan: string) ->
    fun m ->
        ChatServer.Types.ChannelMsg(chan, m) |> ApplicationMsg |> ChatDataMsg |> dispatch)

let mainArea = elmishView "MainArea" NoMemoization <| fun { model = model; dispatch = dispatch; } ->
    div [ ClassName "col-xs-12 col-md-8 fs-chat" ]
        [
            (match model.currentPage, model.chat with
            | Route.Overview, _ -> Overview.View.root

            | Channel chan, Connected { serverData = { Channels = channels } } when channels |> Map.containsKey chan ->
                let dispatchChannelMessage = getDispatchChannelMessage (dispatch, chan)
                Channel.View.channel { model = channels.[chan]; dispatch = dispatchChannelMessage }

            | _ ->
                div [] [ str "bad channel route" ])
        ]

let private getMenuDispatch = memoizeOnce (fun (dispatch: App.Types.Msg -> unit) ->
    ApplicationMsg >> ChatDataMsg >> dispatch)

let mutable stableDispatch = None

let root model dispatch =
    let dispatch =
        match stableDispatch with
        | None ->
            stableDispatch <- Some dispatch
            dispatch
        | Some x -> x

    let menu = NavMenu.View.menu {
        chatData = NavMenu.View.menuModelFromServer model.chat
        currentPage = model.currentPage
        dispatch = getMenuDispatch dispatch
    }

    div [ ClassName "container" ]
        [ menu
          mainArea { model = model; dispatch = dispatch; }]