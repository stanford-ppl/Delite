package ppl.delite.framework


trait DeliteCollection[T] {
  def size: Int
  def dcApply(idx: Int) : T
  def dcUpdate(idx: Int, x: T)
}