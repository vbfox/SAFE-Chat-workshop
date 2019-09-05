module Channel.State

open Elmish
open Types
open Browser.Dom

let init () : Model * Cmd<Msg> =
    { NextNonUserMessageId = 1; Users = Map.empty; Messages = []; MessageCount = 0; PostText = ""; Info = ChannelInfo.Empty}, Cmd.none

let init2 (chan: ChannelInfo, users: UserInfo list) : Model * Cmd<Msg> =
    { (fst <| init()) with Info = chan; Users = users |> List.map (fun u -> u.Id, u) |> Map.ofList }, Cmd.none

let getUserNick userid users =
    users |> Map.tryFind userid |> Option.map (fun user -> user.Nick)

let unknownUser userId () = {
    Id = userId; Nick = "Unknown #" + userId; Status = ""
    IsBot = false; Online = true; ImageUrl = None; isMe = false}

let mapUser users userId =
    users
    |> Map.tryFind userId
    |> Option.defaultWith (unknownUser userId)

let mapMessage { Id = id; Ts = ts; Content = text} author =
    { Id = id; Ts = ts; Content = UserMessage (text, author) }

let private maxListLength (len: int) (lst: list<_>) =
    let listLength = lst.Length
    if listLength > len then
        lst |> List.skip (listLength - len), listLength
    else
        lst, listLength

let private maxMessages = 1000

let update (msg: Msg) state: (Model * Msg Cmd) =

    match msg with
    | Init (info, userlist, messagelist) ->
        let users = userlist |> List.map (fun u -> u.Id, u) |> Map.ofList
        let limitedMessageList, messageCount = maxListLength maxMessages messagelist
        let messages = limitedMessageList |> List.map (fun (u, msg) -> mapMessage msg (mapUser users u))
        in
        { state with Info = info; Messages = messages;  MessageCount = messageCount; PostText = ""; Users = users }, Cmd.none
    | Update info ->
        { state with Info = info }, Cmd.none

    | AppendMessage message ->
        let messages, messageCount = maxListLength maxMessages (state.Messages @ [message])
        { state with Messages = messages; MessageCount = messageCount }, Cmd.none

    | AppendUserMessage (userId, message) ->
        let author = mapUser state.Users userId
        let message = mapMessage message author
        let message, nextId =
            if message.Id = 0 then
                { message with Id = -state.NextNonUserMessageId }, state.NextNonUserMessageId + 1
            else
                message, state.NextNonUserMessageId
        let messages, messageCount = maxListLength maxMessages (state.Messages @ [message])

        { state with NextNonUserMessageId = nextId; Messages = messages; MessageCount = messageCount }, Cmd.none

    | UserJoined user ->
        let systemMessage = {
            Id = -state.NextNonUserMessageId; Ts = System.DateTime.Now
            Content = SystemMessage <| sprintf "%s joined the channel" user.Nick }

        let messages, messageCount = maxListLength maxMessages (state.Messages @ [systemMessage])
        { state with
            NextNonUserMessageId = state.NextNonUserMessageId + 1
            Messages = state.Messages @ [systemMessage]
            MessageCount = messageCount
            Users = state.Users |> Map.add user.Id user}, Cmd.none

    | UserUpdated user ->
        let appendMessage =
            state.Users |> getUserNick user.Id |> function
            | Some oldnick when oldnick <> user.Nick ->
                let txt = sprintf "%s is now known as %s" oldnick user.Nick
                [{  Id = -state.NextNonUserMessageId; Ts = System.DateTime.Now; Content = SystemMessage txt }]
            | _ -> []

        let messages, messageCount = maxListLength maxMessages (state.Messages @ appendMessage)
        { state with
            NextNonUserMessageId = state.NextNonUserMessageId + 1
            Messages = state.Messages @ appendMessage
            MessageCount = messageCount
            Users = state.Users |> Map.add user.Id user}, Cmd.none

    | UserLeft userId ->
        let appendMessage =
            state.Users |> getUserNick userId |> function
            | Some oldnick ->
                let txt = sprintf "%s left the channel" oldnick
                [{  Id = -state.NextNonUserMessageId; Ts = System.DateTime.Now; Content = SystemMessage txt }]
            | _ -> []

        let messages, messageCount = maxListLength maxMessages (state.Messages @ appendMessage)
        { state with
            NextNonUserMessageId = state.NextNonUserMessageId + 1
            Messages = state.Messages @ appendMessage
            MessageCount = messageCount
            Users = state.Users |> Map.remove userId}, Cmd.none

    | SetPostText text ->
        {state with PostText = text}, Cmd.none

    | PostText ->
        match state.PostText with
        | text when text.Trim() <> "" ->
            {state with PostText = ""}, Cmd.ofMsg (Forward text)
        | _ ->
            state, Cmd.none

    | Leave
    | Forward _ ->
        console.error (sprintf "%A message is not expected in channel update." msg)
        state, Cmd.none
