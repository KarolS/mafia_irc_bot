package stasiak.karol.mafiaircbot.conspiracy

import stasiak.karol.mafiaircbot.common.AbstractBot
import stasiak.karol.mafiaircbot.common.GameStatus._
import scala.util.Random


class ConspiracyBot(_sendPrivateMessage: (String, Seq[String])=>Unit) extends AbstractBot[ConspiracyGame](_sendPrivateMessage, new ConspiracyGame(_)) {
  
  def poke(sender: String) = List("Poking not yet supported in this mode.")
  
  def reallyEndRunningGame() = List("Sorry, ending a running game not yet supported.")
  
  def status() = game match{
    case None => List("There is no game.")
    case Some(g) => 
      g.signedupPlayers.toList.sortBy(identity).map{ player =>
        s"$player is signed up."
      }.toList
  }
  
  def theEnd(g:ConspiracyGame) = {
    if(g.roundsLeft==0){
      val correctResult = Some(g.against.isDefined)
      val innocents = g.against match {
        case None => g.signedupPlayers.toList.sortBy(identity)
        case Some(dude) => List(dude)
      }
      
      g.status = Over
      
      val innocentsResults = innocents.map{ inn =>
        inn + (g.votes.get(inn) match {
          case None => " didn't vote"
          case Some(true) => " voted that there is a conpiracy"
          case Some(false) => " voted that there is no conspiracy"
        }) + " and has " +(
          if(g.votes.get(inn)==correctResult) "won"
          else "lost"
        ) + " the game."
      }.toList
      
      val conspiratorResults = g.against.toList.flatMap{ against =>
        val wonLost = if(g.votes.get(against)==correctResult) "lost" else "won"
        g.signedupPlayers.toList.filter(_!=against).map{ conspirator =>
          s"${conspirator} (a conspirator) has ${wonLost} the game" + {
            if(g.votes.get(conspirator)==Some(false) && wonLost == "won") " and is sincerely surprised."
            else "."
          }
        }
      }
      
      "Game over" +: (innocentsResults++conspiratorResults).sortBy(identity) 
    } else {
      g.roundsLeft -= 1
      val vs = g.votes
      g.votes = Map()
      "End of the round!" +: g.signedupPlayers.toList.sortBy(identity).map{ player =>
        player + (vs.get(player) match {
          case None => " didn't vote"
          case Some(true) => " voted that there is a conpiracy"
          case Some(false) => " voted that there is no conspiracy"
        }) + "."
      }
    }
  }
  
  def vote(actor:String, vote:Option[Boolean]):List[String] = {
    val reply = game match {
      case None => List("There is no game at the moment.")
      case Some(g) => g.status match {
        case Signups =>
          sendPrivateMessage(actor, Seq("The game has not started yet."))
        case Over =>
          sendPrivateMessage(actor, Seq("The game is over."))
        case Running =>
          g.votes -= actor
          vote.foreach {v=> g.votes += actor->v}
          sendPrivateMessage(actor, Seq("Okay."))
          if(g.votes.size==g.signedupPlayers.size){
            return theEnd(g)
          } else if(vote.isDefined){
            return List(g.votes.size+" playes voted.")
          }
      }
    }
    Nil
  }
  
  def start(playerName: String, descriptor: String, openSetup: Boolean) =  {
    game match {
      case Some(g) => g.status match{
        case Signups =>
          if(g.owner!=playerName){
            sendMessage(playerName, "It's not your game, wait for your turn!",openSetup)
          } else if(g.signedupPlayers.contains(playerName) && !openSetup){
            sendPrivateMessage(playerName, List("You're playing in this game, you have to start it publicly."))
            Nil
          } else{
            val NUMREG = "^([0-9]+)$".r 
            val RANDOM_NUMREG = "^random([0-9]+)$".r 
            val shouldStart = descriptor match {
              case NUMREG(n) =>
                try{
                  g.startGame(n.toInt)
                  true
                } catch {
                  case _ => false 
                }
              case RANDOM_NUMREG(n) =>
                try{
                  g.startGame(Random.nextInt(n.toInt+1))
                  true
                } catch {
                  case _ => false 
                }
              case _ =>
                false
            }
            if(shouldStart){ 
              for (player <- g.signedupPlayers){
                val knownInnocent = g.against.getOrElse(player)
                if(knownInnocent!=player){
                  sendPrivateMessage(player, Seq(
                      s"You are a part of the conspiracy against $knownInnocent",
                      "Convince them that there is no conspiracy.",
                      "Send here 'yes' or 'no' when you would know if there is conspiracy if you were innocent."))
                } else {
                  sendPrivateMessage(player, Seq(
                      "You are innocent!", 
                      "If you know if there is a conspiracy, send here 'yes' or 'no'.",
                      "Send 'retract' to retract your vote."
                      ))
                }
              }
              s"Game started by $playerName." +: "Player list:" +: (
              if(g.signedupPlayers.size<6) List(g.signedupPlayers.mkString(", "))
              else g.signedupPlayers.toList.sortBy(identity).grouped(3).map(_.mkString(", ")).toList )
            } else {
              List("Accepted descriptors (N is a positive number): N, randomN")
            }
          }
        case Running => sendMessage(playerName,"There is a game running at the moment.", openSetup)
        case Over => sendMessage(playerName,"Create a new game using !create first.", openSetup)
      } 
      case None =>
        sendMessage (playerName, "There is no game to start.", openSetup)
    }
  }
}