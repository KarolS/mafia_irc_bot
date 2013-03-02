package stasiak.karol.mafiaircbot.common

import scala.util.Random
import GameStatus._
import stasiak.karol.mafiaircbot.mafia.Game

object Silliness {
  val MANE6 = Seq("Twilight Sparkle", "Fluttershy", "Pinkie Pie", "Rainbow Dash", "Rarity", "Applejack")
  def mane6() = MANE6(Random.nextInt(6))
  val CMC = Seq("Apple Bloom", "Scootaloo", "Sweetie Belle")
  def cmc() = CMC(Random.nextInt(6))
  
  val PONIES = MANE6 ++ CMC
  def poni() = PONIES(Random.nextInt(PONIES.size))
  
  def roll(diceCount: String, diceSize: String) = {
    if(diceSize==0) "There's no 0-sided dice" 
    else {
      try{
        if(diceCount.toInt>3000) "I'm not rolling that many dice, do it yourself."
        else {
          val result = (0 until diceCount.toInt).map{_ => Random.nextInt(diceSize.toInt) + 1}.sum.toString
          s"${diceCount}d$diceSize: $result"
        }
      } catch {
        case _:NumberFormatException => "Some of those numbers are too big."
      }
    }
  }
  
  def coinflip() = if(Random.nextBoolean){
    "Result of a coin toss: HEADS."
  }else {
    "Result of a coin toss: TAILS."
  }
  
  def nowkiss(game: Option[AbstractGame]) = game match { 
    case None => "Sadly, there is no game."
    case Some(g) => g.status match {
      case Over => "Sadly, the game is over."
      case _ => if(g.signedupPlayers.size < 2) {
        "There are not enough players."
      } else {
        val player1 = g.signedupPlayers.toList apply Random.nextInt(g.signedupPlayers.size)
        val player2 = (g.signedupPlayers-player1).toList apply Random.nextInt(g.signedupPlayers.size - 1)
        s"$player1 and $player2 kiss."
      }
    }
    
  }
}