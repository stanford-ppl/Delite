// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


/**
 * FF test harness
 */
class UIntAccumTests(c: UIntAccum) extends PeekPokeTester(c) {
  var init = 0
  var count = 0
  poke(c.io.init, init)
  step(1)

  for(i <- 0 until 50) {
    val acc = rnd.nextInt(10)
    val en = rnd.nextInt(2)
    val next = c.tp match {
      case "add" => count + acc  
      case "max" => if(count > acc) count else acc  
      case "min" => if(count < acc) count else acc
    }
    count = if(en == 1) next else count
    poke(c.io.next, acc)
    poke(c.io.enable, en)
    step(1)
    expect(c.io.output, count)
  }
  poke(c.io.enable, 0)
  step(3)
  poke(c.io.reset, 1)
  step(1)
  poke(c.io.reset, 0)
  step(1)

  count = 0
  for(i <- 0 until 50) {
    val acc = rnd.nextInt(10)
    val en = rnd.nextInt(2)
    val next = c.tp match {
      case "add" => count + acc  
      case "max" => if(count > acc) count else acc  
      case "min" => if(count < acc) count else acc
    }
    count = if(en == 1) next else count
    poke(c.io.next, acc)
    poke(c.io.enable, en)
    step(1)
    expect(c.io.output, count)
  }


  step(5)
}
