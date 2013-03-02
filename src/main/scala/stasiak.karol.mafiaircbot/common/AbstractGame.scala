package stasiak.karol.mafiaircbot.common

object GameStatus extends Enumeration {
  type GameStatus = Value
  val Signups, Running, Over = Value
}
import GameStatus._
abstract class AbstractGame(val owner: String) {
  var status = Signups
  var signedupPlayers = Set[String]()
  var askedForEnding = Set[String]()
  var endGameAt = Long.MaxValue
  
}