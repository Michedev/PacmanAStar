import scala.collection.mutable.MutableList

abstract class Cell
case class Wall() extends Cell
case class Food() extends Cell
case class Pacman() extends Cell
case class Floor() extends Cell


class Node(private var _typeCell: Cell = Wall()) {
  private val _neightbours = new Array[Node](4)
  def neighbours = _neightbours

  private var _explored: Boolean = false

  def explored: Boolean = _explored

  def explored_=(value: Boolean): Unit = {
    _explored = value
  }


  def topNeightbour = _neightbours(0)
  def leftNeightbour = _neightbours(1)
  def bottomNeightbour = _neightbours(2)
  def rightNeightbour = _neightbours(3)

  def topNeightbour_= (node: Node) = _neightbours(0) = node
  def leftNeightbour_= (node: Node) = _neightbours(1) = node
  def bottomNeightbour_= (node: Node) = _neightbours(2) = node
  def rightNeightbour_= (node: Node) = _neightbours(3) = node

	def typeCell = _typeCell
	def typeCell_= (newType: Cell) = if(_typeCell.isInstanceOf[Wall] ) { _typeCell = newType }

	private var _position = (-1, -1)
	def position = _position
	def position_= (newPosition : (Int, Int)) = if( _position == (-1, -1)) { _position = newPosition }

  override def toString: String = _position.toString() + "- Type: "+ _typeCell.toString + " #Neightbours = "+ _neightbours.count(_ != null)

  override def equals(o: scala.Any): Boolean =
  {
    o match {
      case o: Node =>
         o.position._1 == _position._1 && o.position._2 == _position._2 &&
         (o._typeCell.getClass.getSimpleName equals _typeCell.getClass.getSimpleName)
      case _ =>  super.equals(o)
    }

  }
}
