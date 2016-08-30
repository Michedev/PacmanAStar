

trait AStar[T] {
  def f(x : T) : Int = g(x) + h(x)
  def g(x : T) : Int
  def h(x: T) : Int
}