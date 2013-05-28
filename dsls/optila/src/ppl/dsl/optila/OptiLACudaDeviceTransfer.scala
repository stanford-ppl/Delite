package ppl.dsl.optila

import virtualization.lms.internal.{Expressions, CudaDeviceTransfer, CudaCodegen}

trait OptiLACudaDeviceTransfer extends CudaDeviceTransfer {
  this: CudaCodegen =>
}
