package stasiak.karol.mafiaircbot

import stasiak.karol.mafiaircbot.mafia.Bot
import stasiak.karol.mafiaircbot.mafia.MafiaParser
import stasiak.karol.mafiaircbot.dummy.EmptyParser
import stasiak.karol.mafiaircbot.conspiracy.ConspiracyParser

object Main {

  def main(args: Array[String]): Unit = {
    val conn:Connection = new IrcConnection(args.toList)
    
    conn.msgChannel(List("Hello scum!",
      "The Great And Powerful Mafia Bot is here!",
      "Type !mafiahelp to receive basic information about how to interact with the bot.",
      "Type !modes to see all available bot modes and !mode MODE_NAME to switch modes."))
    
    val parsers = Map(
        "mafia" -> new MafiaParser(conn),
        "off" -> new EmptyParser(conn),
        "cons" -> new ConspiracyParser(conn)
        ) 
    var parser = parsers("mafia")
    
    val MODE_START = "^!mode +([^ ]+)$".r
    conn.listenForever{m=>
      try {
        m match {
          case ChanMsg(_, "!modes") => 
            conn.msgChannel("Available modes: "+:parsers.map{
              case (name, parser) => s"${parser.name} ($name)"
            }.toList)
          case PrivMsg(sender, "!modes") => 
            conn.msgUser(sender, "Available modes: "+:parsers.map{
              case (name, parser) => s"${parser.name} ($name)"
            }.toList)
          case ChanMsg(_, MODE_START(mode)) =>
            if(parsers.contains(mode)){
              if(parser.canSwitch){
                val newParser = parsers(mode)
                if(parser ne newParser){
                  parser = newParser
                  conn.msgChannel("Mode switched to "+parser.name)
                }
              } else {
                conn.msgChannel("Please wait for the current game to finish.")
              }
            } else {
              conn.msgChannel("Unsupported mode: "+mode)
            }
          case m => parser.parse(m)
        }
      }catch {
        case e:Exception => conn.msgChannel(e.toString()) 
        throw e
      }
    } 
  }

}
