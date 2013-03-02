package stasiak.karol.mafiaircbot.conspiracy

import stasiak.karol.mafiaircbot.common.AbstractGame
import stasiak.karol.mafiaircbot.common.GameStatus._
import scala.util.Random

class ConspiracyGame(_owner: String)extends AbstractGame(_owner){
  
  var against:Option[String] = None
  var votes = Map[String,Boolean]()
  var roundsLeft = 0
  def startGame(rounds: Int){
    status = Running
    roundsLeft = rounds
    against = if(signedupPlayers.size>=1){
      Random.nextInt(signedupPlayers.size+1) match {
        case 0 => None //no conspiracy
        case i => Some(signedupPlayers.toList(i-1))
      }
    } else None //no conspiracy if only one player
  }
}