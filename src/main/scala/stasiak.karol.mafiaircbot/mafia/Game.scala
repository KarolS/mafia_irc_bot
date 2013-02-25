package stasiak.karol.mafiaircbot.mafia
import java.util.Locale

object GameStatus extends Enumeration {
  type GameStatus = Value
  val Signups, Running, Over = Value
}
import GameStatus._
import Alignment._
class Game(val owner: String){
  var status = Signups
  var signedupPlayers = Set[String]()
  var publicNightEvents = List[String]()
  var players = Map[String,Player]()
  var votes = Map[String,String]()
  var mafiaTargets: Option[List[(String, String)]] = None
  var werewolfTargets: Option[List[(String, String)]]= None
  var night = false
  var mafiaNightKills = 1
  var werewolfNightKills = 1
  var jesterKillRatio = 0.5
  var askedForEnding = Set[String]()
  var endGameAt = Long.MaxValue
  def newNight(){
    night = true
    publicNightEvents = Nil
    mafiaTargets = None
    werewolfTargets = None
    askedForEnding=Set()
    endGameAt = Long.MaxValue
    players.values.foreach(_.prepareForNight())
  }
  def newDay() = {
    val minOrder = players.values.map(_.role.order).min
    val maxOrder = players.values.map(_.role.order).max
    for( o <- minOrder to maxOrder ; (name,player) <- players; if player.alive && player.role.order == o){
      player.process(this)
    }
    (mafiaTargets.getOrElse(Nil)++werewolfTargets.getOrElse(Nil)).map(_._2).foreach{ target =>
      players(target).health -= 1
    }
    for((name,player) <- players ; if player.alive){
      if(player.health<0){
        player.alive = false
        player.nightResults = List("You have been killed.")
        publicNightEvents :+= s"$name has been killed."
      } else {
        if(player.target != Nil && player.shots < 1000000){
          player.nightResults :+= s"You have ${player.shots} shots left."
        }
      }
    }
    night = false
    votes = Map()
    askedForEnding=Set()
    endGameAt = Long.MaxValue
    players.values.foreach(_.prepareForDay())
    publicNightEvents
  }
  
  def canSwitchPhase()={
    if(status != Running) {
      false
    }else if(night){
      val mafiaIsAlive = players.values.exists(p=>p.alive && p.role.alignment == Mafia)
      val werevolvesAreAlive = players.values.exists(p=>p.alive && p.role.alignment == Werewolves)
      if(System.currentTimeMillis() > this.endGameAt) true
      else if(mafiaIsAlive && mafiaTargets.isEmpty) false
      else if(werevolvesAreAlive && werewolfTargets.isEmpty) false
      else players.values.forall(p=>p.doneHisTurn(this))
    } else {
      players.values.filter(_.alive).forall(_.vote.isDefined) || {
        val votes = players.values.filter(_.alive).collect{_.vote match { case Some(i) => i}}
        val maxVoteCount = (votes.groupBy(identity).map{case (_,vs)=>vs.size}.toList:+0).max
        maxVoteCount*2 > players.values.filter(_.alive).size
      } 
    }
  }
  def hasGameEnded() = {
    players.values.forall(p=>p.role.gameResult(p, this).isDefined)
  }
  def normalizeName(shortName:String)={
    if(shortName == "") Some("")
    else players.keys.filter(_.toLowerCase(Locale.US) == shortName.toLowerCase(Locale.US)).toList match {
      case List(perfectMatch) => Some(perfectMatch)
      case _ => players.keys.filter(_.toLowerCase(Locale.US).startsWith(shortName.toLowerCase(Locale.US))).toList match {
        case List(theOnlyOne) => Some(theOnlyOne)
        case _ => None
      }
    }
  }
}