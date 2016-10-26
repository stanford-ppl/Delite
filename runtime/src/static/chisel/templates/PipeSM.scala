import Chisel._

class PipeSM extends Module {

    // States
    val pipeInit :: pipeReset :: pipeRun :: pipeDone :: pipeSpinWait :: Nil = Enum(UInt(), 5)

    // Module IO
    val io = new Bundle {
      
      // State machine IO
      val sm_done = Bool(OUTPUT)
      val sm_en = Bool(INPUT)

      // Counter IO
      val ctr_done = Bool(INPUT)
      val ctr_en = Bool(OUTPUT)

      // Reset IO
      val rst_done = Bool(INPUT)
      val rst_en = Bool(OUTPUT)

      // TODO - Implement counter iterations
      // private final DFEsmInput[] sm_maxIn;
      // private final DFEsmOutput[] ctr_maxOut;

    }

    // Defaults
    io.sm_done := Bool(false)
    io.ctr_en := Bool(false)
    io.rst_en := Bool(false)

    // Initialize state
    val state = Reg(init = pipeInit)
    
    // Only start the state machine when the enable signal is set
    when (io.sm_en)
    {
      // Change states
      switch (state) {

        // INIT state
        is (pipeInit) {

          // Next state
          state := pipeReset
        }

        // RESET state
        is (pipeReset) {

          // Set the reset enable
          io.rst_en := Bool(true) & ~io.rst_done;
          
          // Next state
          when (io.rst_done) {
            state := pipeRun
          }

        }

        // RUN state
        is (pipeRun) {

          // Set the counter enable
          io.ctr_en := Bool(true) & ~io.ctr_done;

          // Next state
          when (io.ctr_done) {
            state := pipeDone
          }
          .otherwise {
            state := pipeRun
          }
        }

        // DONE state
        is (pipeDone) {

          // Set the done signal
          io.sm_done := Bool(true)

          // Next state
          state := pipeSpinWait
        }

        //SPIN_WAIT state
        is (pipeSpinWait) {
            state := pipeSpinWait;
        } 

      }
    }

}
