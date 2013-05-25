package ppl.dsl.optiml

import virtualization.lms.internal.{Expressions, CudaDeviceTransfer, CudaCodegen}

trait OptiMLCudaDeviceTransfer extends CudaDeviceTransfer {
  this: CudaCodegen =>
}
