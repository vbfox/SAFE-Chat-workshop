module Diag

open Akka.Actor
open Akkling
open Suave.Logging

open ChatUser
open ChatTypes
open ChatServer

let private logger = Log.create "bot"

let mutable private spamTimer: System.Threading.Timer option = None

let spamText =
    [
        "You sit here, dear."
        "All right."
        "Morning!"
        "Morning!"
        "Well, what've you got?"
        "Well, there's egg and bacon; egg sausage and bacon; egg and spam; egg bacon and spam; egg bacon sausage and spam; spam bacon sausage and spam; spam egg spam spam bacon and spam; spam sausage spam spam bacon spam tomato and spam;"
        "Spam spam spam spam..."
        "...spam spam spam egg and spam; spam spam spam spam spam spam baked beans spam spam spam..."
        "Spam! Lovely spam! Lovely spam!"
        "...or Lobster Thermidor au Crevette with a Mornay sauce served in a Provencale manner with shallots and aubergines garnished with truffle pate, brandy and with a fried egg on top and spam."
        "Have you got anything without spam?"
        "Well, there's spam egg sausage and spam, that's not got much spam in it."
        "I don't want ANY spam!"
        "Why can't she have egg bacon spam and sausage?"
        "THAT'S got spam in it!"
        "Hasn't got as much spam in it as spam egg sausage and spam, has it?"
        "Spam spam spam spam (crescendo through next few lines)"
        "Could you do the egg bacon spam and sausage without the spam then?"
        "Urgghh!"
        "What do you mean 'Urgghh'? I don't like spam!"
        "Lovely spam! Wonderful spam!"
        "Shut up!"
        "Lovely spam! Wonderful spam!"
        "Shut up! (Vikings stop) Bloody Vikings! You can't have egg bacon spam and sausage without the spam."
        "I don't like spam!"
        "Sshh, dear, don't cause a fuss. I'll have your spam. I love it. I'm having spam spam spam spam spam spam spam beaked beans spam spam spam and spam!"
        "Spam spam spam spam. Lovely spam! Wonderful spam!"
        "Shut up!! Baked beans are off."
        "Well could I have her spam instead of the baked beans then?"
        "You mean spam spam spam spam spam spam... (but it is too late and the Vikings drown her words)"
        "Spam spam spam spam. Lovely spam! Wonderful spam! Spam spa-a-a-a-a-am spam spa-a-a-a-a-am spam. Lovely spam! Lovely spam! Lovely spam! Lovely spam! Lovely spam! Spam spam spam spam! "
    ]

/// Creates an actor for echo bot.
let createEchoActor (getUser: GetUser) (system: ActorSystem) (botUserId: UserId) (chan: ChannelData) =
    let mutable spamLine = 0
    let onTimer _ =
        let msg = spamText.[spamLine]
        spamLine <- (spamLine + 1) % spamText.Length
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
