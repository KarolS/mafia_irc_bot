package stasiak.karol.mafiaircbot.mafia

object Alignment extends Enumeration {
  type Alignment = Value
  val Town, Mafia, Werewolves, Survival, Hostile = Value
  
  def mkString(v: Alignment.Value):String = v match {
    case Town => "town"
    case Mafia => "mafia"
    case Werewolves => "werewolf"
    case _ => "independent"
  }
  
  def simplify(v1: Value) = v1 match {
    case Survival => Hostile
    case x => x
  }
}

import Alignment._

sealed trait Role {
  def canTargetAlive = true
  def canTargetDead = false
  def canTargetSelf = false
  def isADangerousTownie = false
  def isAKillingRole = false
  def shots = Int.MaxValue
  def order: Int
  def numberOfTargets: Int
  def minimalNumberOfTargets = numberOfTargets
  def jesterness = 0
  def process(player: Player, game: Game)
  def hasToReply(player: Player, game: Game) = {
    if(numberOfTargets <= 0 || player.shots<=0 || player.target.size>=minimalNumberOfTargets) false
    else if (player.alive == false) false
    else if (canTargetAlive==false && game.players.values.forall(_.alive)) false
    else true
  }
  def knows(otherRole: Role) = alignment match {
    case Mafia => otherRole.alignment == Mafia
    case Werewolves => otherRole.alignment == Werewolves
    case _ => false
  }
  def gameResult(player: Player, game: Game): Option[Boolean] = {
    val towniesLive = game.players.values.exists(p => p.alive && p.role.alignment==Town)
    val mafiaLive = game.players.values.exists(p => p.alive && p.role.alignment==Mafia)
    val hostilesLive = game.players.values.exists(p => p.alive && p.role.alignment==Hostile)
    val werewolvesLive = game.players.values.exists(p => p.alive && p.role.alignment==Werewolves)
    val survivalistsLive = game.players.values.exists(p => p.alive && p.role.alignment==Survival)
    alignment match {
      case Town =>
        if(towniesLive){
          if(mafiaLive || hostilesLive || werewolvesLive) {
            if(hostilesLive) {
              None
            } else if (mafiaLive && werewolvesLive || survivalistsLive) {
              None
            } else { //one scum faction left, no survivalists
              val scumCount = game.players.values.count{
                p => p.alive && 
                (p.role.alignment==Mafia || p.role.alignment==Werewolves)
              }
              val aliveCount = game.players.values.count(p => p.alive)
              if(scumCount*2>=aliveCount) Some(false)
              else None
            }
          }
          else Some(true)
        } else Some(false)
      case Mafia =>
        if(mafiaLive){
          if(towniesLive) {
            if(survivalistsLive || hostilesLive || werewolvesLive || game.players.values.exists(p => p.alive && p.role.isADangerousTownie)) {
              None
            } else {
              val mafiaCount = game.players.values.count(p => p.alive && p.role.alignment==Mafia)
              val aliveCount = game.players.values.count(p => p.alive)
              if(mafiaCount*2>=aliveCount) Some(true)
              else None
            }
          }
          else {
            if(hostilesLive || werewolvesLive) None
            else Some(true)
          }
        } else Some(false)
      case Werewolves =>
        if(werewolvesLive){
          if(towniesLive) {
            if(survivalistsLive || hostilesLive || mafiaLive || game.players.values.exists(p => p.alive && p.role.isADangerousTownie)) {
              None
            } else {
              val werewolfCount = game.players.values.count(p => p.alive && p.role.alignment==Werewolves)
              val aliveCount = game.players.values.count(p => p.alive)
              if(werewolfCount*2>=aliveCount) Some(true)
              else None
            }
          }
          else {
            if(hostilesLive || mafiaLive) None
            else Some(true)
          }
        } else Some(false)
      case Hostile =>
        if(player.alive) {
          if(game.players.values.count(_.alive) == 1) Some(true)
          else None
        } else Some(false)
      case Survival =>
        if(player.alive){
          if(game.players.values.exists(p=>p.role.alignment != Survival && p.role.gameResult(p, game)==None)) None
          else Some(true)
        } else Some(false)
    }
  }
  
  def voteStrength = 1 //unused yet
  
  def alignment: Alignment
  
  def name: String
  
  def realName = name
  
  
  protected def skills = "you possess no special abilities."
  
  def flavour = (alignment match {
    case Mafia | Werewolves => "Each night, anyone from your team can choose a targets for factional kill. You win when all hostiles die and you outumber townsfolk. Apart from that, "
    case Town => "You are town-aligned and you win when you get rid of all hostiles. Apart from that, "
    case Hostile => "You win when you get rid of all other players. Apart from that, "
    case Survival => "You win if you survive to the end of the game. Apart from that, "
  }) + skills
    
  protected def roleblock(target: String, game: Game){
    if(target!=""){
      game.players(target).target = Nil
      game.mafiaTargets.foreach {mt => game.mafiaTargets = Some(mt.filter(_._1==target))}
      game.werewolfTargets.foreach {mt => game.werewolfTargets = Some(mt.filter(_._1==target))}
    }
  }
}

/*************************************
 ABSTRACT COMMON ROLES
*************************************/

abstract class PassiveRole extends Role {
  val order = 0
  val numberOfTargets = 0
  def process(player: Player, game: Game) {}
}

abstract class BombRole extends Role {
  val order = 1
  val numberOfTargets = 0
  override def isADangerousTownie = alignment == Town
  def process(player: Player, game: Game) {
    if(player.health<0){
      game.players.values.filter{ p=>
        p.alive && p.role.isAKillingRole && p.target.contains(player.name)
      }.foreach(_.health -= 1)
      (game.mafiaTargets++game.werewolfTargets).foreach{
        _.filter(_._2==player.name).foreach{case(scum,bomb) => game.players(scum).health -= 1}
      }
    }
  }
}

abstract class PgoRole extends Role {
  val order = 1
  val numberOfTargets = 0
  def process(player: Player, game: Game) {
    game.players.values.filter{ p=>
      p.alive && p.target.contains(player.name)
    }.foreach(_.health -= 1)
    (game.mafiaTargets++game.werewolfTargets).foreach{
      _.filter(_._2==player.name).foreach{case(scum,bomb) => game.players(scum).health -= 1}
    }
  }
}
//TODO: Drivers vs Roleblockers

abstract class DriverRole extends Role {
  val order = -2
  val numberOfTargets = 2
  def drive(target1:String, target2: String)(target: String) = {
    if(target==target1) target2
    else if (target==target2) target1
    else target
  }
  def process(player: Player, game: Game) {
    player.target match {
      case List("", _) | List(_,"") | Nil=> ()
      case List(t1,t2) => 
        val driveF = drive(t1,t2)_
        game.players.values.foreach{ p=>
          p.target = p.target.map(driveF)
        }
        game.mafiaTargets = game.mafiaTargets.map(_.map{case (x,y)=> (x,driveF(y))})
        game.mafiaTargets = game.mafiaTargets.map(_.map{case (x,y)=> (x,driveF(y))})
    }
  }
  override val skills = "each night, you can swap actions targeting two alive players of your choice."
}

abstract class RoleblockerRole extends Role {
  val order = -3
  val numberOfTargets = 1
  def process(player: Player, game: Game) {
    player.target.map{ targetName =>
      if(targetName!="") {
        game.players(targetName).target=Nil
        game.mafiaTargets = game.mafiaTargets.map(_.filterNot(_._1 == targetName))
        game.werewolfTargets = game.werewolfTargets.map(_.filterNot(_._1 == targetName))
      }
    }
  }
  override val skills = "every night, you can target one alive player and block their night action."
}

abstract class GunsmithRole extends Role {
  val order = 0
  val numberOfTargets = 1
  def process(player: Player, game: Game) {
    player.nightResults = player.target.filter(_!="").map{ targetName =>
      game.players(targetName).role match {
        case Mafioso | Godfather | _:PgoRole | _:VigilanteRole =>
          s"${targetName} has a gun."
        case _ =>
          s"${targetName} doesn't have any gun."
      }
    }.toList
  }
  override val skills = "every night, you can target one alive player and learn if they have a gun." 
}

abstract class VigilanteRole extends Role{
  def shots:Int
  val order = 0
  val numberOfTargets = 1
  val alignment = Town
  override val isADangerousTownie = true
  override val isAKillingRole = true
  def process(player: Player, game: Game) {
    player.target.map{ targetName =>
      if(targetName!="") game.players(targetName).health -= 1
    }
  }
  val name = "Vigilante"
  protected def skills:String
}
abstract class JesterRole extends PassiveRole {
  val alignment = Survival
  def jesterness:Int
  override def gameResult(player: Player, game: Game) = {
    if(player.alive) {
      super.gameResult(player, game).map(_=>false)
    } else Some(player.roleRevealed)
  }
  val name = "Jester"
  override val flavour = "You win only if you are lynched before the end of the game."
}

/*************************************
 TOWN
*************************************/

case object VanillaTownie extends PassiveRole {
  val alignment = Town
  val name = "Vanilla Townie"
}
case object Earl extends PassiveRole {
  val alignment = Town
  val name = "Earl"
}
case object Miller extends PassiveRole {
  val alignment = Town
  val name = "Miller"
}
case object TownDriver extends DriverRole {
  val alignment = Town
  val name = "Town-aligned Driver"
}
case object TownRoleblocker extends RoleblockerRole {
  val alignment = Town
  val name = "Town-aligned Roleblocker"
}
case object TownGunsmith extends GunsmithRole {
  val alignment = Town
  val name = "Town-aligned Gunsmith"
}
case object Mortician extends Role {
  val order = 0
  val numberOfTargets = 1
  override val canTargetAlive = false
  override val canTargetDead = true
  val alignment = Town
  val name = "Mortician"
    
  def process(player: Player, game: Game) {
    player.nightResults = player.target.filter(_!="").map{targetName => 
      s"$targetName was a ${game.players(targetName).role.name}."
    }
  }
  override val skills = "each night, you can check the role of one dead player."
}
case object Cop extends Role {
  val order = 0
  val numberOfTargets = 1
  val alignment = Town
  def process(player: Player, game: Game) {
    player.nightResults = player.target.filter(_!="").map{targetName => 
      val result = game.players(targetName).role match {
        case Miller =>
          Mafia
        case Godfather =>
          Town
        case x =>
          x.alignment
      }
      result match {
        case Town => s"$targetName is town-aligned."
        case Mafia => s"$targetName is mafia-aligned."
        case Werewolves => s"$targetName is werewolf-aligned."
        case Hostile | Survival => s"$targetName is self-aligned."
      }
    }
  }
  val name = "Cop"
  override val skills = "each night, you can check one alive player's alignment."  
}

case object Doctor extends Role {
  val order = 0
  val numberOfTargets = 1
  val alignment = Town
  def process(player: Player, game: Game) {
    player.target.map{ targetName =>
      if(targetName!="") game.players(targetName).health += 1
    }
  }
  val name = "Doctor"
  override val skills = "each night, you can protect one alive player from one kill."  
}
case object Mason extends PassiveRole {
  val alignment = Town
  val name = "Mason"
  override def knows(otherRole: Role) = otherRole == Mason
}

case object Watcher extends Role {
  val order = 0
  val numberOfTargets = 1
  val alignment = Town
  def process(player: Player, game: Game) {
    player.nightResults = player.target.filter(_!="").flatMap{ watchee =>
      val peopleTargetingTheStandardWay = game.players.flatMap {
        case (otherName, otherPlayer) =>
          if(otherPlayer.target.contains(watchee)) List(otherName)
          else Nil
      }
      val peopleTargetingTheScumWay = 
        (game.mafiaTargets.getOrElse(Nil) ++ game.werewolfTargets.getOrElse(Nil)).filter(_._2 == watchee).map(_._1)
      val watcheeResults = (peopleTargetingTheScumWay++peopleTargetingTheStandardWay).sortBy(identity).map{ targeter =>
        s"$watchee was targeted by $targeter."
      }
      if(watcheeResults.isEmpty){
        List(s"$watchee was not targeted by anyone.")
      } else watcheeResults
    }
  }
  val name = "Watcher"
  override val skills = "each night, you can observe one alive player and see who targets them that night."  
}
case object Tracker extends Role {
  val order = 0
  val numberOfTargets = 1
  val alignment = Town
  def process(player: Player, game: Game) {
    player.nightResults = player.target.filter(_!="").flatMap{ trackee =>
      val peopleTargetedTheStandardWay = game.players(trackee).target
      val peopleTargetedTheScumWay = 
        (game.mafiaTargets.getOrElse(Nil) ++ game.werewolfTargets.getOrElse(Nil)).filter(_._1 == trackee).map(_._2)
      val trackeeResults = (peopleTargetedTheScumWay++peopleTargetedTheStandardWay).sortBy(identity).map{ targetee =>
        s"$trackee targeted $targetee."
      }
      if(trackeeResults.isEmpty){
        List(s"$trackee did not target anyone.")
      } else trackeeResults
    }
  }
  val name = "Tracker"
  override val skills = "each night, you can observe one alive player and see whom they are targeting that night."  
}
case object Vigilante2Shot extends VigilanteRole{
  override val shots = 2
  override val skills = "twice at night, you can kill one player"
}
case object Vigilante1Shot extends VigilanteRole{
  override val shots = 1
  override val skills = "once at night, you can kill one player"
}
case object Bomb extends BombRole {
  val alignment = Town
  val name = "Bomb"
  override val skills = "you kill anyone who kills you at night."
}

case object Pgo extends PgoRole {
  val alignment = Town
  val name = "Paranoid Gun Owner"
  override val skills = "you kill anyone who targets you at night."
}
/*************************************
 WEREWOLVES
*************************************/

abstract class WerewolfRole extends Role{
  val alignment = Werewolves
  def process(player: Player, game: Game) {}
  def order = 0
  def numberOfTargets = 0
}

case object Werewolf extends WerewolfRole {
  val name = "Werewolf"
}
/*************************************
 MAFIA
*************************************/

abstract class MafiaRole extends Role{
  val alignment = Mafia
  def process(player: Player, game: Game) {}
  def order = 0
  def numberOfTargets = 0
}
case object Mafioso extends MafiaRole {
  val name = "Mafioso"
}
case object Godfather extends MafiaRole {
  val name = "Godfather"
}

case object MafiaDriver extends DriverRole{
  val alignment = Mafia
  val name = "Mafia-aligned Driver"
}
case object MafiaRoleblocker extends RoleblockerRole{
  val alignment = Mafia
  val name = "Mafia-aligned Roleblocker"
}
case object MafiaGunsmith extends GunsmithRole {
  val alignment = Mafia
  val name = "Mafia-aligned Gunsmith"
}

/*************************************
 INDEPENDENTS
*************************************/

case object Jester extends JesterRole {
  override val jesterness = 50
}
case object Jester0 extends JesterRole {
  override val jesterness = 0
}
case object Jester100 extends JesterRole {
  override val jesterness = 120
}
case object Jester66 extends JesterRole {
  override val jesterness = 66
}
case object SerialKiller extends Role {
  val order = 0
  val numberOfTargets = 1
  override val canTargetSelf = true
  override val isAKillingRole = true
  val alignment = Hostile
  def process(player: Player, game: Game) {
    player.target.map{ targetName =>
      if(targetName!="") game.players(targetName).health -= 1
    }
  }
  val name = "Serial Killer"
  override val skills = "each night, you can kill one player."
}
case object Survivalist extends PassiveRole {
  val alignment = Survival
  val name = "Survivalist"
}
case object SerialTownie extends Role {
  val order = 0
  val numberOfTargets = 0
  val alignment = Hostile
  def process(player: Player, game: Game) {}
  val name = "Serial Townie"
}
