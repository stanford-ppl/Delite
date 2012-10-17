package ppl.dsl.optiml

import virtualization.lms.internal.{Expressions, CudaDeviceTransfer, CudaCodegen}

trait OptiMLCudaDeviceTransfer extends CudaDeviceTransfer {
  this: CudaCodegen =>

  val IR: Expressions
  import IR._

  override def emitSendSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case _ => super.emitSendSlave(tp)
    }
  }

  override def emitRecvSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case _ => super.emitRecvSlave(tp)
    }
  }

  //TODO: Implement view for slave devices
  /*
  override def emitSendViewSlave(tp: Manifest[Any]): (String,String) = {
  }

  override def emitRecvViewSlave(tp: Manifest[Any]): (String,String) = {
  }
  */

  override def emitSendUpdateSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case _ => super.emitSendUpdateSlave(tp)
    }
  }

  override def emitRecvUpdateSlave(tp: Manifest[Any]): (String,String) = {
    remap(tp) match {
      case _ => super.emitRecvUpdateSlave(tp)
    }
  }

}
