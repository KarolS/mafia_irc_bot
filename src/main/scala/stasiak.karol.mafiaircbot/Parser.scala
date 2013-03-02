package stasiak.karol.mafiaircbot

trait Parser {
  def name: String
  def canSwitch: Boolean
  def parse(msg: Message):Unit
}