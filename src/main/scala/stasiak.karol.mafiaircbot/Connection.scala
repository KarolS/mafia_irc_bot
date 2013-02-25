package stasiak.karol.mafiaircbot

abstract class Connection {
  
  def msgChannel(msg: String){
    msgChannel(Seq(msg))
  }
  
  def msgUser(user: String, msg: String){
    msgUser(user, Seq(msg))
  }

  def listenForever(handler: Message=>Unit){
    listenWhile{ x=>
      handler(x)
      true
    }
  }
  
  def msgChannel(msgs: Seq[String])
  
  def msgUser(user: String, msgs: Seq[String])
  
  def listenWhile(handler: Message=>Boolean)
  
}