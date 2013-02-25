package stasiak.karol.mafiaircbot


object IrcMessage {
  val END_OF_MOTD = "^[^ ]* 376 ".r
  val PING1 = "^PING :(.*)$".r
  val PING2 = "^:[^ ]* PING :(.*)$".r
  val PRIVMSG = "^:([^!]+)![^ ]* PRIVMSG [^#][^ ]* :(.*)$".r
  val CHANMSG = "^:([^!]+)![^ ]* PRIVMSG #[^ ]* :(.*)$".r
  def apply(raw: String) = raw match {
    case END_OF_MOTD() => EndOfMotd
    case PING1(p) => Ping(p)
    case PING2(p) => Ping(p)
    case PRIVMSG(s,c) => PrivMsg(s,c)
    case CHANMSG(s,c) => ChanMsg(s,c)
    case m => UnsupportedMessage(m) 
  }
}
sealed trait Message {

}
case class Ping (val content: String)extends Message
case class PrivMsg (val sender: String, val content: String) extends Message
case class ChanMsg (val sender: String, val content: String) extends Message
case object EndOfMotd extends Message
case class UnsupportedMessage(raw: String) extends Message