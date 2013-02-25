package stasiak.karol.mafiaircbot

import stasiak.karol.mafiaircbot.mafia.Bot
import stasiak.karol.mafiaircbot.mafia.Silliness

object Main {

  def main(args: Array[String]): Unit = {
    val conn:Connection = new IrcConnection(args.toList)
    val bot = new Bot(conn.msgUser _)
    
    val HELP = Seq(
            "Help for interacting with The Great And Powerful Mafia bot:",
            "Use !create to create a new game.",
            "Use !join to join that game.",
            "Use !leave to leave that game.",
            "Use !start RULESET_DESCRIPTOR to start a game you created.",
            "Use !vote PLAYER to vote during day.",
            "Use !votes to display votes.",
            "Use !nolynch to vote against lynching.",
            "Use !unvote to retract your vote.",
            "Use !players to display player list.",
            "Use !roles to display role list.",
            "Use !poke to poke inactive players.",
            "Use !end to ask the bot to cancel the game or end the phase prematurely.",
            "Use !role ROLE_NAME to display role info",
            "PM the bot with your night targets separated with spaces if applicable.",
            "PM the bot with !kill SCUM VICTIM SCUM VICTIM ... to choose targets for scum.",
            "If scum has only one target and you send !kill VICTIM it will assume you as the perpetrator.",
            "PM the bot with !kill to pass on your factional kill.",
            "PM the bot with !pass to pass on your individual night action."
            )
    val DICE_REGEX = """^\!([0-9]+)d([0-9]+)$""".r
    val START_REGEX = """^\!start +([^ ].*)$""".r
    conn.listenForever{m=>
      try {
        m match{
          case ChanMsg(sender,content) =>
            if(content.startsWith("!vote ")){
              conn.msgChannel(bot.vote(sender, content.substring("!vote ".length)))
            }
            if(content.startsWith("!v ")){
              conn.msgChannel(bot.vote(sender, content.substring("!v ".length)))
            }
            if(content.startsWith("!lynch ")){
              conn.msgChannel(bot.vote(sender, content.substring("!lynch ".length)))
            }
            if(content.startsWith("!role ")){
              conn.msgChannel(bot.role(content.substring("!role ".length)))
            }

            content match {
              case "!join" | "!j" => conn.msgChannel(bot.join(sender))
              case "!leave" => conn.msgChannel(bot.leave(sender))
              case "!poke" => conn.msgChannel(bot.poke(sender))
              case "!end" => conn.msgChannel(bot.end(sender))
              case "!create" => conn.msgChannel(bot.create(sender))
              case "!unvote" => conn.msgChannel(bot.unvote(sender))
              case "!votes" => conn.msgChannel(bot.votes())
              case "!roles" => conn.msgChannel(bot.roles())
              case "!nolynch"|"!pass"|"!novote" => conn.msgChannel(bot.vote(sender, ""))
              case "!start" => conn.msgChannel("Use !start RULESET_DESCRIPTOR to start the game.")
              case START_REGEX(descriptor) => conn.msgChannel(bot.start(sender, descriptor, true))
              case "!status" | "!players" => conn.msgChannel(bot.status())
              case "!help" | "!mafiahelp" => conn.msgUser(sender, HELP)
              
              case "!ranking" | "!score" => conn.msgChannel("I disabled it because it was silly.")//bot.ranking())
              case DICE_REGEX(count, size) => conn.msgChannel(Silliness.roll(count,size))
              case "!poni" => conn.msgChannel(Silliness.poni())
              case "!nowkiss" => conn.msgChannel(Silliness.nowkiss(bot.game))
              case _ =>
            }
          case PrivMsg(sender, content) =>
            if(content.startsWith("!start ")){
              conn.msgChannel(bot.start(sender, content.substring("!start ".length), false))
            }
            if(content == ("!start")){
              conn.msgUser(sender, "Use !start RULESET_DESCRIPTOR to start the game.")
            }
            
            //performative
            if(content == "!pass" || content == "!p"){
              conn.msgChannel(bot.nightPass(sender))
            }
            if(content == "!kill" || content == "!k"){
              conn.msgChannel(bot.scumTargets(sender, Nil))
            }
            if(content.startsWith("!kill ")) {
              conn.msgChannel(bot.scumTargets(sender, content.substring("!kill ".length).split(" ").filterNot(_.isEmpty).toList))
            }
            if(content.startsWith("!k ")) {
              conn.msgChannel(bot.scumTargets(sender, content.substring("!k ".length).split(" ").filterNot(_.isEmpty).toList))
            }
            if(!content.startsWith("!")) {
              conn.msgChannel (bot.nightAction(sender, content.split(" ").filterNot(_.isEmpty).toList))
            }
            
            //informative
            if(content.startsWith("!role ")){
              conn.msgUser(sender,bot.role(content.substring("!role ".length)))
            }
            content match {
              case "!help" | "!mafiahelp" | "!h" => conn.msgUser(sender, HELP)
              case "!votes" => conn.msgUser(sender,bot.votes())
              case "!roles" => conn.msgUser(sender,bot.roles())
            }
          case _ =>
        }
      }catch {
        case e:Exception => conn.msgChannel(e.toString()) 
        throw e
      }
    } 
  }

}
