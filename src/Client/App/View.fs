module App.View

open Fable.Websockets.Elmish.Types
open App.Types
open Router
open Connection.Types
open Fable.React
open Props

let root model dispatch =
    let mainArea =
        match model.currentPage, model.chat with
        | Route.Overview, _ -> [ Overview.View.root ]

        | Channel chan, Connected { serverData = { Channels = channels } } when channels |> Map.containsKey chan ->
            let dispatchChannelMessage m = ChatServer.Types.ChannelMsg(chan, m) |> ApplicationMsg |> ChatDataMsg |> dispatch
            Channel.View.root channels.[chan] dispatchChannelMessage

        | _ ->
            [ div [] [ str "bad channel route" ] ]

    let menu = NavMenu.View.menu { chatData = model.chat; currentPage = model.currentPage; dispatch = (ApplicationMsg >> ChatDataMsg >> dispatch) }

    div [ ClassName "container" ]
        [ menu
          div [ ClassName "col-xs-12 col-md-8 fs-chat" ] mainArea ]
