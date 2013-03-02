package stasiak.karol.mafiaircbot.common

import stasiak.karol.mafiaircbot.Parser
import stasiak.karol.mafiaircbot.Connection
import stasiak.karol.mafiaircbot.Message
import stasiak.karol.mafiaircbot.ChanMsg
import stasiak.karol.mafiaircbot.PrivMsg

abstract class AbstractParser[B<:AbstractBot[_<:AbstractGame]](val conn: Connection, val botFactory: ()=>B) extends Parser {
  
  val bot = botFactory()
  
  def canSwitch = bot.game.foldLeft(true)((_,g)=> g.status == GameStatus.Over)
  
  def HELP: Seq[String]
  
  def parseInner(msg: Message):Unit
  
  val DICE_REGEX = """^\!([0-9]+)d([0-9]+)$""".r
  val DICE_REGEX1 = """^\!d([0-9]+)$""".r
  val START_REGEX = """^\!start +([^ ].*)$""".r
  def parse(msg: Message){
    msg match {
      case ChanMsg(sender,content) =>
        content match {
          case "!join" | "!j" => conn.msgChannel(bot.join(sender))
          case "!leave" => conn.msgChannel(bot.leave(sender))
          case "!poke" => conn.msgChannel(bot.poke(sender))
          case "!end" => conn.msgChannel(bot.end(sender))
          case "!create" => conn.msgChannel(bot.create(sender))
          case "!start" => conn.msgChannel("Use !start RULESET_DESCRIPTOR to start the game.")
          case START_REGEX(descriptor) => conn.msgChannel(bot.start(sender, descriptor, true))
          case "!status" | "!players" => conn.msgChannel(bot.status())
          case "!help" | "!mafiahelp" => conn.msgUser(sender, HELP)
          
          case DICE_REGEX(count, size) => conn.msgChannel(Silliness.roll(count,size))
          case DICE_REGEX1(size) => conn.msgChannel(Silliness.roll("1",size))
          case "!poni" => conn.msgChannel(Silliness.poni())
          case "!coinflip"|"!cointoss" => conn.msgChannel(Silliness.coinflip())
          case "!nowkiss" => conn.msgChannel(Silliness.nowkiss(bot.game))
          case _ => parseInner(msg)
        }
      case PrivMsg(sender, content) =>
        content match {
          case START_REGEX(descriptor) => conn.msgChannel(bot.start(sender, descriptor, false))
          case "!start" => conn.msgUser(sender, "Use !start RULESET_DESCRIPTOR to start the game.")
          case "!help" | "!mafiahelp" | "!h" => conn.msgUser(sender, HELP)
          case _ => parseInner(msg)
        }
      case _ =>
        parseInner(msg)
          
    }
  }
}