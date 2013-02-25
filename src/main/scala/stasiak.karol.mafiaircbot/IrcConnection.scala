package stasiak.karol.mafiaircbot

import java.net.Socket
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream

class IrcConnection(params: List[String]) extends Connection{
  private val DIGITS = "^([0-9]+)$".r
  private def generateParamMap(ps: List[String]):Map[String,String] = ps match {
    case "-s" :: server :: xs => generateParamMap(xs) + ("server" -> server)
    case "-p" :: DIGITS(port) :: xs => generateParamMap(xs) + ("port" -> port)
    case "-n" :: nick :: xs => generateParamMap(xs) + ("nick" -> nick)
    case "-c" :: channel :: xs => generateParamMap(xs) + ("channel" -> {
      if(channel.startsWith("#"))channel else "#"+channel
      })
    case "-u" :: user :: xs => generateParamMap(xs) + ("user" -> user)
    case "-C" :: chpassword :: xs => generateParamMap(xs) + ("chpassword" -> chpassword)
    case "-P" :: password :: xs => generateParamMap(xs) + ("password" -> password)
    case "-A" :: nickserv :: xs => generateParamMap(xs) + ("nickserv" -> nickserv)
    case "-M" :: message :: xs => generateParamMap(xs) + ("message" -> message)
    case Nil => Map()
    case flag::_ => throw new IllegalArgumentException("Invalid flag: "+flag)
  }
  private[this] val paramMap = generateParamMap(params)
  val server = paramMap.getOrElse("server", throw new IllegalArgumentException("No server specified."))
  val port = paramMap.get("port").map(_.toInt).getOrElse(6667)
  val channel = paramMap.getOrElse("channel", throw new IllegalArgumentException("No channel specified."))
  val channelPassword: Option[String] = paramMap.get("chpassword")
  
  val socket = new Socket(server, port)
  val ins = new BufferedReader(new InputStreamReader(socket.getInputStream)) 
  val outs = new PrintStream(socket.getOutputStream)
  
  
  
  def sender(msg:String) {
    this.synchronized{
      println("> " + msg)
      outs.print(msg+(13.toChar.toString)+(10.toChar.toString))
    }
  }
  
  private[this] var _name = ""

  // initialisation: 
  nick(paramMap.getOrElse("nick", throw new IllegalArgumentException("No nick specified.")))
  user( paramMap("nick"),  paramMap.getOrElse("user", "The Great And Powerful Mafia Bot"))
  paramMap.get("password").foreach{
    pass(_)
  }
  Thread.sleep(5000)
  if(paramMap.keys.exists(_=="nickserv") ^ paramMap.keys.exists(_=="message")){
    throw new IllegalArgumentException("You need to specify both authentication message and nickserv username, or none")
  } else {
    paramMap.get("nickserv").foreach{
      msgUser(_, paramMap("message"))
    }
  }

  channelPassword match {
    case Some(p) =>join(p)
    case None => join()
  }
  msgChannel("Hello scum!")
  msgChannel("The Great And Powerful Mafia Bot is here!")
  msgChannel("Type !mafiahelp to receive basic information about how to interact with the bot.")

    
    
  def name = _name
  def nick(n:String){
    sender("NICK "+n)
    _name = n
  }
  def pass(n:String){
    sender("PASS "+n)
  }
  
  def user(name:String, realName: String) {
    sender(s"USER $name 0 * :$realName")
  }
  
  def join(){
    sender("JOIN :"+channel)
  }
  def join(password: String){
    sender("JOIN "+channel+" "+password)
  }
  
  def msgChannel(msgs: Seq[String]){
    msgs.foreach{
      case "<delay>" => Thread.sleep(500)
      case msg => sender(s"PRIVMSG $channel :$msg")
    }
  }
  
  def msgUser(user: String, msgs: Seq[String]){
    msgs.foreach{
      case "<delay>" => Thread.sleep(500)
      case msg => sender(s"PRIVMSG $user :$msg")
    }
  }
  
  
  def waitForEndOfMotd() {
    listenWhile{
      case EndOfMotd =>
        false
      case _ =>
        true
    }
  }

  def listenWhile(handler: Message=>Boolean){
    var running = true
    while(running){
      Thread.sleep(10)
      val line = ins.readLine()
      if(line != null){
        println ("< "+line)
        val message = IrcMessage(line)
        message match{
          case Ping(p) => 
            sender("PONG :"+p)
          case other => 
            running = handler(other)
        }
      }
    }
  }
  
}