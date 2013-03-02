package stasiak.karol.mafiaircbot.dummy

import stasiak.karol.mafiaircbot.Parser
import stasiak.karol.mafiaircbot.Message
import stasiak.karol.mafiaircbot.Connection

class EmptyParser(conn: Connection) extends Parser{
  def name = "Off"
  def parse(m: Message) {}
  def canSwitch = true
}