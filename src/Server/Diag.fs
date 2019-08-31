module Diag

open Akka.Actor
open Akkling
open Suave.Logging

open ChatUser
open ChatTypes
open ChatServer

let private logger = Log.create "bot"

let mutable private spamTimer: System.Threading.Timer option = None

/// Creates an actor for echo bot.
let createEchoActor (getUser: GetUser) (system: ActorSystem) (botUserId: UserId) (chan: ChannelData) =
    let onTimer _ =
        let msg = "Lovely spam! Wonderful spam!"
        //logger.debug (Message.eventX "Sending spam to {chan}" >> Message.setFieldValue "chan" chan.cid)
        chan.channelActor <! ChannelCommand (PostMessage (botUserId, Message msg))
        ()

    spamTimer <- Some (new System.Threading.Timer(onTimer, null, 0, 100))

    let getPersonNick {identity = identity; nick = nick} =
        match identity with
        |Person _
        |Anonymous _
            -> Some nick
        | _ -> None

    let forUser userid fn = async {
        let! user = getUser userid
        return user |> Option.bind getPersonNick |> Option.map fn
    }

    let handler (ctx: Actor<_>) =
        let rec loop () = actor {
            let! msg = ctx.Receive()
            let! reply =
                match msg with
                | ChatMessage { author = author; message = Message message} ->
                    forUser author (fun nickName -> sprintf "%s said: %s" nickName message)
                | Joined { user = user} ->
                    forUser user (fun nickName -> sprintf "Welcome aboard, %s!" nickName)
                | _ -> async.Return None

            match reply with
            | Some reply -> do ctx.Sender() <! ChannelCommand (PostMessage (botUserId, Message reply))
            | _ -> ()

            return! loop()
        }
        loop()
    in
    spawn system "echobot" <| props(handler)

let createDiagChannel (getUser: GetUser) (system: ActorSystem) (server: IActorRef<_>) (echoUserId, channelName, topic) =
    async {
        let chanActorProps = GroupChatChannelActor.props None

        let! result = server |> getOrCreateChannel channelName topic (OtherChannel chanActorProps)
        match result with
        | Ok chanId ->
            let! channel = server |> getChannel (fun chan -> chan.cid = chanId)
            match channel with
            | Ok chan ->
                let bot = createEchoActor getUser system echoUserId chan
                chan.channelActor <! ChannelCommand (NewParticipant (echoUserId, bot))
            | Result.Error _ ->
                () // FIXME log error
        | Result.Error _ ->
            () // FIXME log error
    }
