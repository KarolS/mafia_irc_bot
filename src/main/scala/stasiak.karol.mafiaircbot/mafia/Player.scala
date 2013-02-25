package stasiak.karol.mafiaircbot.mafia

class Player(val name:String , val role: Role) {
  var alive = true
  var roleRevealed = false
  var health = 0
  var target = List[String]()
  var vote:Option[String] = None
  var nightResults = List[String]()
  var shots = role.shots
  def prepareForDay() {
    vote = None
  }
  def prepareForNight(){
    target = Nil
    nightResults = Nil
    health = 0
  }
  def hasWon(game: Game) = role.gameResult(this, game) == Some(true)
  
  def hasToReply(game: Game) = role.hasToReply(this, game)
  def doneHisTurn(game: Game) = !role.hasToReply(this, game)
  def process(game:Game) = {
    if(shots>0){
      shots -= 1
      role.process(this, game)
    }
  }
  def order = role.order
}