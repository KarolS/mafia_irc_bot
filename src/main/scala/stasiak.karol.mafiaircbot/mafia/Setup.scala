package stasiak.karol.mafiaircbot.mafia
import scala.util.Random
import java.util.Locale

import stasiak.karol.mafiaircbot.common.GameStatus

object Setup {
  def apply(descriptor: String) = descriptor match {
    case x if x.startsWith("/") => try{
      Some(new StandardSetup(x.substring(1)))
    } catch {
      case _ => None
    }
    case "test" => Some(TestSetup)
    case x => try{
      Some(new StandardSetup(x))
    } catch {
      case _ => None
    }
  }
  val roleDictionary = Map[String,Role](
      "vt" -> VanillaTownie,
      "vanilla" ->VanillaTownie,
      "earl" -> Earl,
      "mi" -> Miller,
      "miller" -> Miller,
      "ma" -> Mason,
      "mason" -> Mason,
      
      "d" -> Doctor,
      "doc" -> Doctor,
      "doctor" -> Doctor,
      "cop" -> Cop,
      "c" -> Cop,
      "mo" -> Mortician,
      "mortician" -> Mortician,
      
      "w" -> Watcher,
      "watcher" -> Watcher,
      "t" -> Tracker,
      "tracker" -> Tracker,
      "towndriver" -> TownDriver,
      "td" -> TownDriver,
      "towngunsmith" -> TownGunsmith,
      "tgs" -> TownGunsmith,
      "townroleblocker" -> TownRoleblocker,
      "trb" -> TownRoleblocker,
      
      "v2" -> Vigilante2Shot,
      "vigilante2" -> Vigilante2Shot,
      "v1" -> Vigilante1Shot,
      "vigilante1" -> Vigilante1Shot,
      "bomb" -> Bomb,
      "b" -> Bomb,
      "pgo" -> Pgo,
      "granny" -> Pgo,

      "m" -> Mafioso,
      "mafioso" -> Mafioso,
      "gf" -> Godfather,
      "godfather" -> Godfather,
      "mafiadriver" -> MafiaDriver,
      "md" -> MafiaDriver,
      "mafiagunsmith" -> MafiaGunsmith,
      "mgs" -> MafiaGunsmith,
      "mafiaroleblocker" -> MafiaRoleblocker,
      "mrb" -> MafiaRoleblocker,

      "werewolf" -> Werewolf,
      "ww" -> Werewolf,

      "sk" -> SerialKiller,
      "killer" -> SerialKiller,
      "serialkiller" -> SerialKiller,
      "surv" -> Survivalist,
      "survivalist" -> Survivalist,
      "j" -> Jester,
      "jester" -> Jester,
      "j50" -> Jester,
      "jester50" -> Jester,
      "j0" -> Jester0,
      "jester0" -> Jester0,
      "j66" -> Jester66,
      "jester66" -> Jester66,
      "j100" -> Jester100,
      "jester100" -> Jester100

  )
}
trait Setup {
  def roles(playerCount:Int):List[Role] 
  def startsWithNight: Boolean
  def minimalPlayerNumber: Int
  def maximalPlayerNumber: Int
  def initialiseGame(game: Game){
    val count = game.signedupPlayers.size
    game.night = startsWithNight
    game.status = GameStatus.Running
    game.players = game.signedupPlayers.toList.zip(Random.shuffle(roles(count))).map{
      case(name, role) => 
        val p = new Player(name,role)
        name -> p
    }.toMap
  }
}
class StandardSetup(descriptor: String) extends Setup{
  val potentialSetups = descriptor.split("\\|").map{ alternative =>
    alternative.split(",").flatMap{theSameRoles =>
      if(theSameRoles.contains("*")){
        val Array(number, role) = theSameRoles.split("\\*")
        List.fill(number.trim.toInt)(Setup.roleDictionary(role.trim.toLowerCase(Locale.US)))
      }else {
        List(Setup.roleDictionary(theSameRoles.trim.toLowerCase(Locale.US)))
      }
    }.toList
  }.toArray
  val minimalPlayerNumber = potentialSetups.map(_.length).max
  val maximalPlayerNumber = minimalPlayerNumber * 10
  val startsWithNight = false
  def roles(playerCount:Int) = {
    val partialSetup = potentialSetups(Random.nextInt(potentialSetups.length))
    partialSetup ++ List.fill(playerCount-partialSetup.length)(VanillaTownie)
  }
}
object TestSetup extends Setup{
  val minimalPlayerNumber = 1
  val maximalPlayerNumber = 100000
  def roles(playerCount:Int) = if(playerCount<3){
    List.fill(playerCount)(SerialTownie)
  } else {
    Mafioso :: Cop :: Doctor :: List.fill(playerCount-3)(VanillaTownie)
  }
  val startsWithNight = false
}