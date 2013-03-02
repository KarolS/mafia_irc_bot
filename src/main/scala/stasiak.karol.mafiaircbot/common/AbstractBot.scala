package stasiak.karol.mafiaircbot.common
import GameStatus._
abstract class AbstractBot[G<:AbstractGame](val sendPrivateMessage: (String, Seq[String])=>Unit, val gameFactory: String=>G) {
  var game: Option[G] = None
  
  def create(playerName: String) = {
    game match {
      case Some(g) =>
        if (g.status == Over) {
          game = Some(gameFactory(playerName))
          s"Game created by $playerName."
        } else "There is a game running at the moment."
      case None =>
        game = Some(gameFactory(playerName))
        s"Game created by $playerName."
    }
  }  
  
  def join(playerName: String) = {
    game match {
      case None => "There is no game at the moment."
      case Some(g) => g.status match {
        case Signups =>
          g.signedupPlayers += playerName
          val playerCount = 
            s"${if(g.signedupPlayers.size==1)"is"else"are"} ${g.signedupPlayers.size} player${if(g.signedupPlayers.size==1)""else"s"}"
          s"$playerName signed up for the game. There $playerCount now."
        case Running =>
          "The game has already started."
        case Over =>
          "The game is over."
      }
    }
  }
  def leave(playerName: String) = {
    game match {
      case None => "There is no game at the moment."
      case Some(g) => g.status match {
        case Signups =>
          if(g.signedupPlayers.contains(playerName)){
            g.signedupPlayers -= playerName
            val playerCount = 
              s"${if(g.signedupPlayers.size==1)"is"else"are"} ${g.signedupPlayers.size} player${if(g.signedupPlayers.size==1)""else"s"}"
            s"$playerName resigned from playing the game. There are $playerCount now."
          } else {
            s"Silly $playerName, you're not playing in this game."
          }
        case Running =>
          "The game has already started."
        case Over =>
          "The game is over."
      }
    }
  }
  
  def start(sender:String, descriptor: String, openSetup: Boolean): List[String]
  def poke(sender:String): List[String]
  def status(): List[String]
  
  def sendMessage(playerName: String, message: String, isPublic: Boolean) = {
    if(isPublic) List(message)
    else {
      sendPrivateMessage(playerName, List(message))
      Nil
    }
  }
  
  def reallyEndRunningGame():List[String]
  
  def endInternal(g:AbstractGame){
    val x = g.askedForEnding.size.toDouble
    val k = g.signedupPlayers.size.toDouble
    val n = 30000.0 // 30 seconds
    val newEndGameAt = if(x<0.5)Long.MaxValue else System.currentTimeMillis() + (n*(x-k*x+k*k-k)/x).toLong
    if(newEndGameAt<g.endGameAt) g.endGameAt = newEndGameAt
  }
  
  def end(poker: String) = game match{
    case None => List("There is no game at the moment.")
    case Some(g) => g.status match {
      case Over =>
        List("The game is already over, feel free to start a new one.")
      case Signups => 
        if(g.signedupPlayers.contains(poker)){
          if(g.endGameAt<System.currentTimeMillis()){
            game = None
            List("Game cancelled because the creator was tardy and didn't start the game.")
          }else {
            g.askedForEnding += poker
            endInternal(g)
            val players = if(g.askedForEnding.size==1) "1 player" else g.askedForEnding.size+" players"
            val timeLeft = (g.endGameAt-System.currentTimeMillis())/1000
            List(s"$players asked to cancel the game. Please reply with !end after ${timeLeft} seconds pass to cancel it.")
          }
        }else{
          List(s"Silly $poker, you are not signed up for this game.")
        }
      case Running => 
        if(g.signedupPlayers.contains(poker)){
          if(g.endGameAt<System.currentTimeMillis()){
            reallyEndRunningGame()
          }else {
            g.askedForEnding += poker
            endInternal(g)
            val players = if(g.askedForEnding.size==1) "1 player" else g.askedForEnding.size+" players"
            val timeLeft = (g.endGameAt-System.currentTimeMillis())/1000
            List(s"$players asked to speed up the current phase. Please reply with !end after ${timeLeft} seconds pass to force the next phase.")
          }
        }else{
          List(s"Silly $poker, you are not playing in this game.")
        }
     }
  }
}