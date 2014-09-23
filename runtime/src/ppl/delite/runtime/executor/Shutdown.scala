package ppl.delite.runtime.executor

/**
 * This Executable will cause an ExecutionThread to terminate immediately after its execution
 */

final class Shutdown(thread: ExecutionThread) extends DeliteExecutable {

  def run() {
    thread.continue = false
  }

}
