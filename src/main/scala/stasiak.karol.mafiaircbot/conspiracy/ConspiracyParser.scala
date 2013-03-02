package stasiak.karol.mafiaircbot.conspiracy

import stasiak.karol.mafiaircbot.common.AbstractParser
import stasiak.karol.mafiaircbot.Connection
import stasiak.karol.mafiaircbot.common.GameStatus
import stasiak.karol.mafiaircbot.PrivMsg
import stasiak.karol.mafiaircbot.Message

class ConspiracyParser(_conn: Connection) extends AbstractParser[ConspiracyBot](_conn, ()=>new ConspiracyBot(_conn.msgUser _)) {
  
  val name = "Conspiracy"
    
  val HELP = Seq(
            "Help for interacting with The Great And Powerful Mafia bot in Conspiracy mode:",
            "Use !create to create a new game.",
            "Use !join to join that game.",
            "Use !leave to leave that game.",
            "Use !start RULESET_DESCRIPTOR to start a game you created (currently the descriptor can by anything).",
            "Use !players to display player list.",
            "Use !end to ask the bot to cancel the game.",
            "PM the bot with yes, no or retract."
      )
  def parseInner(m: Message){
    m match{
      case PrivMsg(sender,content) =>
        content.toLowerCase.trim match {
          case "yes" => conn.msgChannel(bot.vote(sender, Some(true)))
          case "no" => conn.msgChannel(bot.vote(sender, Some(false)))
          case "retract" => conn.msgChannel(bot.vote(sender, None))
          case "!yes" => conn.msgChannel(bot.vote(sender, Some(true)))
          case "!no" => conn.msgChannel(bot.vote(sender, Some(false)))
          case "!retract" => conn.msgChannel(bot.vote(sender, None))
          case _ =>
        }
      case _ =>
    }
  }
}