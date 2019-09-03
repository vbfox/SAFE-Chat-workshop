module Channel.View

open Fable.Core.JsInterop
open Fable.React

open Props
open Types

open Fable.ReactMarkdownImport

let private formatTs (ts: System.DateTime) =
  match (System.DateTime.Now - ts) with
  | diff when diff.TotalMinutes < 1.0 -> "a few seconds ago"
  | diff when diff.TotalMinutes < 30.0 -> sprintf "%i minutes ago" (int diff.TotalMinutes)
  | diff when diff.TotalHours <= 12.0 -> ts.ToShortTimeString()
  | diff when diff.TotalDays <= 5.0 -> sprintf "%i days ago" (int diff.TotalDays)
  | _ -> ts.ToShortDateString()

let inline valueOrDefault value =
    Ref <| (fun e -> if e |> isNull |> not && !!e?value <> !!value then e?value <- !!value)

let splitter = elmishView "Splitter" ByRef <| fun () ->
    div [ ClassName "fs-splitter" ] []

let messageInput dispatch model =
    fragment [] [
      splitter ()
      div
        [ ClassName "fs-message-input" ]
        [ input
            [ Type "text"
              Placeholder "Type the message here..."
              valueOrDefault model.PostText
              OnChange (fun ev -> !!ev.target?value |> (SetPostText >> dispatch))
              OnKeyPress (fun ev -> if !!ev.which = 13 || !!ev.keyCode = 13 then dispatch PostText)
            ]
          button
            [ ClassName "btn" ]
            [ i [ ClassName "mdi mdi-send mdi-24px"
                  OnClick (fun _ -> dispatch PostText) ] [] ]
        ]
    ]

let chanUsers (users: Map<string, UserInfo>) =
  let screenName (u: UserInfo) =
    match u.IsBot with |true -> sprintf "#%s" u.Nick |_ -> u.Nick
  div [ ClassName "userlist" ]
      [ str "Users:"
        ul []
          [ for u in users ->
              li [] [str <| screenName u.Value]
          ]]

type ChannelInfoProps = {
    chan: ChannelInfo
    dispatch: Msg -> unit
}

let chatInfo = elmishView "ChannelInfo" ByRef <| fun { chan = chan; dispatch = dispatch; } ->
    fragment [] [
      div
        [ ClassName "fs-chat-info" ]
        [ h1
            [] [ str chan.Name ]
          span
            [] [ str chan.Topic ]
          button
            [ Id "leaveChannel"
              ClassName "btn"
              Title "Leave"
              OnClick (fun _ -> dispatch Leave) ]
            [ i [ ClassName "mdi mdi-door-closed mdi-18px" ] []]
        ]
      splitter ()
    ]

let inline message (text: string) =
    reactMarkdown [Source text ]

type UserMessageProps = {
    text: string
    author: UserInfo
    ts: System.DateTime
}

let userMessage = elmishView "UserMessage" ByRef <| fun { text = text; author = author; ts = ts } ->
    div
      [ classList ["fs-message", true; "user", author.isMe ] ]
      [ div
          []
          [
            message text
            h5 []
               [ span [ClassName "user"] [str author.Nick]
                 span [ClassName "time"] [str <| formatTs ts ]] ]
        UserAvatar.View.root author.ImageUrl
      ]

type SystemMessageProps = {
    text: string
    ts: System.DateTime
}

let systemMessage = elmishView "SystemMessage" ByRef <| fun { SystemMessageProps.text = text; ts = ts } ->
    blockquote
      [ ClassName ""]
      [ str text; str " "
        small [] [str <| formatTs ts] ]

type MessageListProps = {
    Messages: Message Envelope list
}

type MessageProps = {
    key: string
    message: Message Envelope
}

let messageElement = elmishView "Message" ByRef <| fun { message = message } ->
    match message.Content with
    | UserMessage (text, author) ->
       userMessage { text = text; author = author; ts = message.Ts }
    | SystemMessage text ->
       systemMessage { text = text; ts = message.Ts }

let messageList (messages: Message Envelope list) =
    let messagesElements =
        messages |> List.map(fun m -> messageElement { key = m.Id.ToString(); message = m })

    div
      [ ClassName "fs-messages" ]
      [ ofList messagesElements ]

type ChannelProps = {
    model: Model
    dispatch: Msg -> unit
}

let channel = elmishView "Channel" ByRef <| fun { model = model; dispatch = dispatch; } ->
    fragment []
      [ chatInfo { chan = model.Info; dispatch = dispatch }
        messageList model.Messages
        messageInput dispatch model
      ]
