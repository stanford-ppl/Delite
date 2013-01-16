package ppl.dsl.optiml

import virtualization.lms.internal.{Hosts, Expressions, CppHostTransfer, CLikeCodegen}

trait OptiMLCppHostTransfer extends CppHostTransfer {
  this: CLikeCodegen =>

  val IR: Expressions
  import IR._

  override def emitSend(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    super.emitSend(tp, host)
  }


  override def emitRecv(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    super.emitRecv(tp, host)
  }

  override def emitSendView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    super.emitSendView(tp, host)
  }


  override def emitRecvView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    super.emitRecvView(tp, host)
  }

  override def emitSendUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    super.emitSendUpdate(tp, host)
  }

  override def emitRecvUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    super.emitRecvUpdate(tp, host)
  }

}
