package ppl.dsl.optisdr.wifi

import ppl.dsl.optisdr._
import ppl.dsl.optila.DenseVector

trait Demodulator16x16 {
  this: OptiSDRApplication =>
  
  val demodulator16x16 = kernel {() => {
    /* val pilotPolarity = Vector[Int](1,1,1,1, -1,-1,-1,1, -1,-1,-1,-1, 1,1,-1,1, -1,-1,1,1, -1,1,1,-1, 1,1,1,1, 1,1,-1,1,
                                              1,1,-1,1, 1,-1,-1,1, 1,1,-1,1, -1,-1,-1,1, -1,1,-1,-1, 1,-1,-1,1, 1,1,1,1, -1,-1,1,1,
                                              -1,-1,1,-1, 1,-1,1,1, -1,-1,-1,1, 1,-1,-1,-1, -1,1,-1,-1, 1,-1,1,1, 1,1,-1,1, -1,1,-1,1,
                                              -1,-1,-1,-1, -1,1,-1,1, 1,-1,1,-1, 1,1,1,-1, -1,1,-1,-1, -1,1,1,1, -1,-1,-1,-1, -1,-1,-1).cycle

    val CYCLIC_PREFIX_LENGTH_TO_DROP = 16
  
    (ofdmSamples: Rep[Stream[Complex]]) => {
      val groupedSamples = ofdmSamples.grouped(80)
      
      groupedSamples.zip(pilotPolarity) {(samples: Rep[Vector[Complex]], flip: Rep[Int]) => {
        val noprefixSamples = samples.drop1(CYCLIC_PREFIX_LENGTH_TO_DROP)
        
        val fftOut = FFT_1D(noprefixSamples)
        
        val pilot_subcarriers = Vector[Complex](fftOut(43), fftOut(57), fftOut(7), fftOut(21)) * Complex(flip,0)
        
        val data_subcarriers = fftOut.slice(38, 42) ++
          fftOut.slice(44, 56) ++
          fftOut.slice(58, 63) ++
          fftOut.slice(1, 6) ++
          fftOut.slice(8, 22) ++
          fftOut.slice(22, 6)
    
        (pilot_subcarriers, data_subcarriers)
      }}
    } */
  }}
}