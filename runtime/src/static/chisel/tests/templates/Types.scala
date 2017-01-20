// See LICENSE.txt for license details.
package types

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


class FixedPointTesterTests(c: FixedPointTester) extends PeekPokeTester(c) {
  step(5)
}
