package stasiak.karol.mafiaircbot.mafia
import stasiak.karol.mafiaircbot.common.GameStatus._
import scala.util.Random
import java.util.Locale
import stasiak.karol.mafiaircbot.common.AbstractBot
class Bot(_sendPrivateMessage: (String, Seq[String])=>Unit) extends AbstractBot[Game](_sendPrivateMessage, new Game(_)) {
 
  var winsAndLosses = Map[String, (Int,Int)]()
  
  def start(playerName: String, setupString: String, openSetup: Boolean): List[String] = {
    game match {
      case Some(g) => g.status match{
        case Signups =>
          if(g.owner!=playerName){
            sendMessage(playerName, "It's not your game, wait for your turn!",openSetup)
          } else if(g.signedupPlayers.contains(playerName) && !openSetup){
            sendPrivateMessage(playerName, List("You're playing in this game, you have to start it publicly."))
            Nil
          } else{
            Setup(setupString) match {
              case None => 
                sendMessage(playerName, "Invalid setup descriptor.", openSetup)
              case Some(setup) => 
                if(g.signedupPlayers.isEmpty) sendMessage(playerName, "There are no players signed up for the game.", openSetup)
                else {
                  setup.initialiseGame(g)
                  g.players.foreach{
                    case (name, player)=>
                      sendPrivateMessage(name, List("You are a "+player.role.name, player.role.flavour) ++ {
                        val allies = g.players.filter{
                          case (otherName, otherPlayer)=>
                            player.role.knows(otherPlayer.role) && otherName != name
                        }.map(_._1).toList
                        if(allies.isEmpty == false){
                          List(s"Your allies are: ${allies.mkString(", ")}.")
                        } else Nil
                      })
                  }
                  s"Game started by $playerName." +: "Player list:" +: (
                  if(g.signedupPlayers.size<6) List(g.signedupPlayers.mkString(", "))
                  else g.signedupPlayers.toList.sortBy(identity).grouped(3).map(_.mkString(", ")).toList ):+ {
                    if(g.night) {
                      "It's night!"
                    } else {
                      "It's day!"
                    }
                  }
                }
            }
          }
        case Running => sendMessage(playerName,"There is a game running at the moment.", openSetup)
        case Over => sendMessage(playerName,"Create a new game using !create first.", openSetup)
      } 
      case None =>
        sendMessage (playerName, "There is no game to start.", openSetup)
    }
  }
    

  def advanceTime(): List[String] = game match {
    case None => Nil
    case Some(g) =>
      if (g.canSwitchPhase() == false) {
        Nil
      } else {
        
        val phaseResults = if (g.night) {
          val publicResults = g.newDay()
          g.players.foreach{
            case (name, player) =>
              sendPrivateMessage(name, player.nightResults)
          }
          "<delay>" +: publicResults
        } else {
          
          //TODO: different vote strengths
          val votes = g.players.values.flatMap(_.vote)
          val votesGrouped: List[(String, Iterable[String])] = votes.groupBy(x => x).toList
          val voteResults: List[(Int, List[(String, Any)])] = votesGrouped.groupBy(_._2.size).toList.sortBy(-_._1)
          val lynchee = voteResults.headOption.flatMap{
            case (voteCount, List((l, _))) =>
              Some(l)
            case (voteCount, List(("",_),(l, _))) =>
              Some(l)
            case (voteCount, List((l, _),("",_))) =>
              Some(l)
            case _ => None
          }
          
          val lynchResults = (lynchee match {
            case Some("") =>
              "Townsfolk has decided that they are not going to lynch anyone."
            case Some(l) =>
              g.players(l).alive = false
              g.players(l).roleRevealed = true
              val grievers = if(g.players(l).role.jesterness > 0){
                g.players.filter{
                  case (name,player) =>
                    player.vote==Some(l) && name!=l
                }.map(_._1).filter(_=>Random.nextInt(100)<g.players(l).role.jesterness)  
              } else Nil
              grievers.foreach(g.players(_).alive = false)
              if(grievers.isEmpty){
                s"$l (${g.players(l).role.name}) has been lynched!"
              } else {
                s"$l (${g.players(l).role.name}) has been lynched and ${grievers.toList.sortBy(identity).mkString(",")} died from grief!"
              }
              
            case None =>
              "You cannot choose whom to lynch, so for tonight you don't lynch anyone"
          }) +: "<delay>" +: g.players.filter(_._2.vote.isDefined).flatMap{
            case (name, player) =>
              player.vote.map{ v =>
                if (v == "") s"$name voted against lynching."
                else s"$name voted to lynch $v."
              }
          }.toList
          
          g.newNight()
          
          lynchResults
        }
        val additionalHeader = if(g.phaseNo>2*g.signedupPlayers.size) {
          g.players.values.foreach(_.alive = false)
          List("Rocks fall, everybody dies.")
        } else {
          Nil
        }
        val gameResults = if (g.hasGameEnded) {
          g.status = Over
          "<delay>" +: "Game over!" +: g.players.map{
            case (name, player) =>
              if (player.hasWon(g)) {
                winsAndLosses += name -> {x:(Int,Int)=>(x._1+1, x._2)}.apply(winsAndLosses.getOrElse(name, 0->0))
                s"$name (${player.role.realName}) has won the game."
              }else {
                winsAndLosses += name -> {x:(Int,Int)=>(x._1, x._2+1)}.apply(winsAndLosses.getOrElse(name, 0->0))
                s"$name (${player.role.realName}) has lost the game."
              }
          }.toList :+ "<delay>" :+ "Thanks for playing!"
        } else {
          if(g.night) List("<delay>", "The sun has set. The night starts.")
          else List("<delay>", "A new day has started.")
        }
        
        phaseResults ++ additionalHeader ++ gameResults ++ advanceTime()

      }
  }

  
  def poke(poker: String) = game match{
    case None => List("There is no game at the moment.")
    case Some(g) => g.status match {
      case Over | Signups => List("The game is not running.")
      case Running => 
        if(g.night){
          g.players.foreach{
            case (name, player) =>
              if(player.hasToReply(g)){
                sendPrivateMessage(name, Seq("Please send in your night action."))
              }
          }
        }else {
          g.players.foreach{
            case (name, player) =>
              if(player.alive && player.vote.isEmpty){
                sendPrivateMessage(name, Seq("Please vote."))
              }
          }
        }
        List("Inactive players poked.")
    }
  }
  
  def reallyEndRunningGame() = 
    ("Phase sped up because some players were tardy and didn't submit their choices in time.") +: advanceTime()
  def gameAction[A](actor: String, action: String, nightAction: Boolean)(impl: Game => List[String]): List[String] = {
    val reply = game match {
      case None => List("There is no game at the moment.")
      case Some(g) => g.status match {
        case Signups =>
          val l = List("The game has not started yet.")
          if(nightAction){
            sendPrivateMessage(actor, l)
            Nil
          } else l
        case Over =>
          val l = List("The game is over.")
          if(nightAction){
            sendPrivateMessage(actor, l)
            Nil
          } else l
        case Running =>
          if (g.night != nightAction) {
            val l = List(s"Silly $actor, it's ${if(nightAction)"day" else "night"}, you cannot $action.")
            if(nightAction){
              sendPrivateMessage(actor, l)
              Nil
            } else l
          } else {
            g.players.get(actor) match {
              case None =>
                val l = List(s"Silly $actor, you don't play in this game.")
                if(nightAction){
                  sendPrivateMessage(actor, l)
                  Nil
                } else l
              case Some(player) =>
                if (!player.alive) {
                  val l = List(s"Silly $actor, you are dead, you cannot $action.")
                  if(nightAction){
                    sendPrivateMessage(actor, l)
                    Nil
                  } else l
                } else {
                  impl(g)
                } 
            }
          }
      }
    }
    reply ++ advanceTime
  }
  
  
  def unvote(voter: String):List[String] = gameAction(voter, "unvote", false){ g =>
    g.players(voter).vote = None
    List(s"$voter retracted his vote.")
  }
  def vote(voter: String, votee: String):List[String] = gameAction(voter, "vote", false){ g =>
    g.normalizeName(votee.trim()) match {
      case Some("") =>
        g.players(voter).vote = Some("")
        List(s"$voter cast his vote for no lynch.")
      case Some(lynchee) =>
        if(g.players(lynchee).alive){
          g.players(voter).vote = Some(lynchee)
          List(s"$voter cast his vote for $lynchee.")
        } else {
          List(s"Silly $voter, $lynchee is already dead.")
        }
      case None =>
        List(s"Silly $voter, $votee does not play in this game.")
    }

  }
  
  def nightAction(playerName: String, targets:List[String]): List[String] = gameAction(playerName, "target", true){ g =>
    val normTargets = targets.map(g.normalizeName(_))
    val actor = g.players(playerName)
    if(actor.shots<=0){
      sendPrivateMessage(playerName, List("You have no shots left.")) 
    } else if(normTargets.contains(None)) {
      sendPrivateMessage(playerName, List("Invalid target."))
    }else {
      val targets = normTargets.map(_.get)
      if (targets.size > actor.role.numberOfTargets) {
        sendPrivateMessage(playerName, List(s"Wrong number of targets. Should be at most ${actor.role.numberOfTargets}."))
      } else if (targets.size < actor.role.minimalNumberOfTargets) {
        sendPrivateMessage(playerName, List(s"Wrong number of targets. Should be at least ${actor.role.minimalNumberOfTargets}."))
      } else if(targets.contains(playerName) && actor.role.canTargetSelf == false){
        sendPrivateMessage(playerName, List("You cannot target yourself."))
      } else if(targets.exists(n => g.players(n).alive && actor.role.canTargetAlive == false)){
        sendPrivateMessage(playerName, List("You cannot target alive people."))
      } else if(targets.exists(n => g.players(n).alive == false && actor.role.canTargetDead == false)){
        sendPrivateMessage(playerName, List("You cannot target dead people."))
      } else {
        actor.target = targets
        sendPrivateMessage(playerName, List(s"You target ${actor.target.mkString(", ")} tonight."))
      }
    }
    Nil
  }
  
  def nightPass(playerName: String): List[String] = gameAction(playerName, "pass", true){ g =>
    val actor = g.players(playerName)
    actor.target = List.fill(actor.role.numberOfTargets)("")
    sendPrivateMessage(playerName, List("You do nothing tonight."))
    Nil
  }
  
  
  def scumPass(playerName: String): List[String] = gameAction(playerName, "target", true){ g=>
    g.players(playerName).role.alignment match {
      case Alignment.Mafia =>
        mafiaTargets(playerName, Nil)
      case Alignment.Werewolves =>
        werewolfTargets(playerName, Nil)
      case _ =>
        sendPrivateMessage(playerName, List("You are not scum, silly!"))
        Nil
    } 
  }
  def scumTargets(playerName:String, targets: List[String]): List[String] = gameAction(playerName, "target", true){ g=>
    g.players(playerName).role.alignment match {
      case Alignment.Mafia =>
        mafiaTargets(playerName, targets)
      case Alignment.Werewolves =>
        werewolfTargets(playerName, targets)
      case _ =>
        sendPrivateMessage(playerName, List("You are not scum, silly!"))
        Nil
    } 
  }
  
  def mafiaTargets(playerName: String, targets: List[String]): List[String] = 
    scumTargetsInternal(playerName, targets, {(g,t)=>g.mafiaTargets = Some(t)}, _.mafiaNightKills,"mafia", Alignment.Mafia)
  def werewolfTargets(playerName: String, targets: List[String]): List[String] = 
    scumTargetsInternal(playerName, targets, {(g,t)=>g.werewolfTargets = Some(t)}, _.werewolfNightKills ,"werewolves", Alignment.Werewolves)
    
  def scumTargetsInternal(
      playerName: String, 
      targets: List[String], 
      setTargets: (Game,List[(String,String)])=>Unit,
      targetCount:Game=>Int,
      scumFactionName: String,
      scumFaction: Alignment.Value
      ): List[String] = gameAction(playerName, "target", true){ g =>
    val normTargets = targets.map(g.normalizeName(_))
    sendPrivateMessage(playerName, List{
      if(normTargets.contains(None)) {
        s"Invalid $scumFactionName/target."
      }else if ((normTargets.length > targetCount(g)*2 || normTargets.length%2 != 0)&&(normTargets.length!=1 || targetCount(g)!=1)) {
        s"Invalid number of $scumFactionName/targets."
      } else {
        val pairs = {
          if(normTargets.length==1) List(playerName, normTargets.head.get)
          else normTargets.map(_.get)
        }.grouped(2).map{case List(mafioso,target) => mafioso->target}.toList
        if(pairs.exists{
          case (mafioso,target) => 
            !g.players(mafioso).alive || 
            g.players(mafioso).role.alignment != scumFaction || 
            !g.players(target).alive
        }){
          s"Invalid $scumFactionName/target."
        } else if(pairs.map{_._1}.toSet.size != pairs.size){
          "Multiple occurences of one player." // TODO: should we actually test for that?
        } else {
          setTargets(g, pairs)
          "Okay."
        }
      }
    })
    Nil
  }
  
  def status(): List[String]= game match{
    case None => List("There is no game.")
    case Some(g) => g.status match {
      case Running => g.players.toList.sortBy(_._1).map{
          case (name, player) =>
            s"$name (${if(player.roleRevealed)player.role.name else "?"}) is ${if(player.alive)"alive"else"dead"}."
        }.toList
      case Over => g.players.toList.sortBy(_._1).map{
          case (name, player) =>
            s"$name (${player.role.realName}) has ${if(player.hasWon(g))"won"else"lost"}."
        }.toList
      case Signups => g.signedupPlayers.toList.sortBy(identity).map{ player =>
          s"$player is signed up."
        }.toList
    }
  }
  
  def roles(): List[String] = 
      "Available roles:" +: {
        Setup.roleDictionary.toList.groupBy(_._2).groupBy(r=>Alignment.simplify(r._1.alignment)).toList.sortBy(_._1).map{
          case (alignment, rolesToSymbols) => 
            val alignmentName = Alignment.mkString(alignment).capitalize
            val roles = rolesToSymbols.toList.sortBy(_._1.name).map{
              case (role, symbolsWithStuff) => role.name+" "+symbolsWithStuff.map(_._1).sortBy(identity).mkString("(",", ",")")
            }.mkString(", ")
            s"$alignmentName: $roles"
        }
      }.toList
  def role(name: String): List[String]= Setup.roleDictionary.get(name.toLowerCase(Locale.US))match{
    case None => List("There is no such role.")
    case Some(r) => List(s"${r.name} (${Alignment.mkString(r.alignment).capitalize})", r.flavour)
  }
  def votes(): List[String]= game match{
    case None => List("There is no game.")
    case Some(g) => g.players.filter(_._2.alive).toList.sortBy(_._1).map{
      case (name, player) =>
        player.vote match {
          case None => s"$name (${if(player.roleRevealed)player.role.name else "?"}) didn't vote."
          case Some("") => s"$name (${if(player.roleRevealed)player.role.name else "?"}) voted against lynching." 
          case Some(p) => s"$name (${if(player.roleRevealed)player.role.name else "?"}) voted to lynch $p." 
        }
    }.toList
  }
  
  def ranking() = {
    val ordered = winsAndLosses.toList.sortBy{
      case (n,(w,l)) => -1001*w+1000*l
    }
    "Ranking:" +: ordered.zipWithIndex.map{
      case ((n,(w,l)), i) => s"${i+1}. $n W:$w L:$l B:${w-l}"
    }
  }
}