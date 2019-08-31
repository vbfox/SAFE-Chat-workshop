module NavMenu.View

open Browser.Dom
open Fable.Core.JsInterop
open Fable.React
open Props

open Router
open Channel.Types
open ChatServer.Types
open Connection.Types

let private menuItem htmlProp name topic isCurrent =
    button
      [ classList [ "btn", true; "fs-channel", true; "selected", isCurrent ]
        htmlProp ]
      [ h1 [] [str name]
        span [] [str topic]]

type private MenuItemChannelProps = {
  key: string
  ch: ChannelInfo
  currentPage: Route
}

let private menuItemChannel = elmishView "ChannelMenuItem" <| fun { ch = ch; currentPage = currentPage } ->
    let targetRoute = Channel ch.Id
    let jump _ = document.location.hash <- toHash targetRoute
    menuItem (OnClick jump) ch.Name ch.Topic (targetRoute = currentPage)

type private MenuItemChannelJoinProps = {
  key: string
  ch: ChannelInfo
  dispatch: ChatServer.Types.Msg -> unit
}

let private menuItemChannelJoin = elmishView "ChannelJoinMenuItem" <| fun { ch = ch; dispatch = dispatch } ->
    let join chid _ = chid |> Join |> dispatch
    menuItem (OnClick <| join ch.Id) ch.Name ch.Topic false

type private UserProps = {
  me: UserInfo
}

let private user = elmishView "User" <| fun { me = me} ->
    div
        [ ClassName "fs-user" ]
        [ UserAvatar.View.root me.ImageUrl
          h3 [Id "usernick"] [str me.Nick]
          span [Id "userstatus"] [ str me.Status]
          button
            [ Id "logout"; ClassName "btn"; Title "Logout"
              OnClick (fun _ -> document.location.href <- "/logoff") ]
            [ i [ ClassName "mdi mdi-logout-variant"] [] ]
         ]

type private UserChannelsProps = {
    channels: Map<ChannelId, Channel.Types.Model>
    newChanName: string option
    currentPage: Route
    dispatch: ChatServer.Types.Msg -> unit
}

let private userChannels = elmishView "UserChannels" <| fun { newChanName = newChanName; channels = channels; currentPage = currentPage; dispatch = dispatch } ->
    let opened, newChanName = newChanName |> function |Some text -> (true, text) |None -> (false, "")

    let channels = [|
      for (_, ch) in channels |> Map.toSeq do
        yield menuItemChannel { ch = ch.Info; currentPage = currentPage; key = ch.Info.Id }
    |]
    fragment [] [
      h2 []
        [ str "My Channels"
          button
            [ ClassName "btn"; Title "Create New"
              OnClick (fun _ -> (if opened then None else Some "") |> (SetNewChanName >> dispatch)) ]
            [ i [ classList [ "mdi", true; "mdi-close", opened; "mdi-plus", not opened ] ] []]
        ]
      input
        [ Type "text"
          classList ["fs-new-channel", true; "open", opened]
          Placeholder "Type the channel name here..."
          DefaultValue newChanName
          AutoFocus true
          OnChange (fun ev -> !!ev.target?value |> (Some >> SetNewChanName >> dispatch) )
          OnKeyPress (fun ev -> if !!ev.which = 13 || !!ev.keyCode = 13 then dispatch CreateJoin)
        ]
      channels |> ofArray
    ]

type private AllChannelsProps = {
    channels: Map<ChannelId, Channel.Types.Model>
    channelList: Map<ChannelId, ChannelInfo>
    dispatch: ChatServer.Types.Msg -> unit
}

let private allChannels = elmishView "AllChannels" <| fun { channels = channels; channelList = channelList; dispatch = dispatch } ->
    let channelList = [|
        for (chid, ch) in channelList |> Map.toSeq do
            if not(channels |> Map.containsKey chid) then
                yield menuItemChannelJoin { dispatch = dispatch; ch = ch; key = ch.Id }
    |]
    fragment [] [
      h2 []
          [ str "All Channels"
            button
              [ ClassName "btn"; Title "Search" ]
              [ i [ ClassName "mdi mdi-magnify" ] []]
          ]
      channelList |> ofArray
    ]


type MenuProps = {
    chatData: Model
    currentPage: Route
    dispatch: ChatServer.Types.Msg -> unit
}

let menu = elmishView "Menu" <| fun { chatData = chatData; currentPage = currentPage; dispatch = dispatch } ->
    div
        [ ClassName "col-md-4 fs-menu" ]
        (match chatData with
        | NotConnected ->
            [ div [] [str "not connected"] ]
        | Connecting _ ->
            [ div [] [str "connecting"] ]
        | Connected { serverData = { Me = me; NewChanName = newChanName; Channels = channels; ChannelList = channelList } } ->
            [
                user { me = me }
                userChannels { newChanName = newChanName; channels = channels; currentPage = currentPage; dispatch = dispatch }
                allChannels { channels = channels; channelList = channelList; dispatch = dispatch }
            ])