import scala.collection.mutable.ListBuffer



case class PacmanSolution(startingPosition : (Int, Int), grid : List[List[Node]], cost: Int, moves : List[(Int, Int)])



class PacmanAStar(private var pacmanData: PacmanData) extends AStar[Node] {
  
	private var currPosition = pacmanData.startingPacmanPosition
  private var currNode = pacmanData.grid(currPosition._1)(currPosition._2)
	private val listMoves = ListBuffer(currPosition)
  private val badSequences = new ListBuffer[ListBuffer[Node]]
  private val currSequence = ListBuffer(currNode)


  def restart(pacmanData: PacmanData) =
  {
    currPosition = pacmanData.startingPacmanPosition
    currNode = pacmanData.grid(currPosition._1)(currPosition._2)
    pacmanData.grid.foreach(row => row.foreach(node => node.explored = false))
    listMoves.clear()
    badSequences.clear()
    currSequence.clear()
  }
	
  override def g(x: Node = null): Int = if(x == null) currSequence.size else currSequence.count(n => n != x)

	//manhattan heuristic
  override def h(x: Node): Int = Math.abs(x.position._1 - currPosition._1) + Math.abs(x.position._2 - currPosition._2)

  val checkIfContain = (badSeq: ListBuffer[ListBuffer[Node]], currSeq: ListBuffer[Node]) => badSeq.exists(seq => (seq.size == currSeq.size) && seq.zipWithIndex.forall(nodeIndex => nodeIndex._1 equals currSeq(nodeIndex._2)))

  private def backtrack() : Unit =
  {
    badSequences += new ListBuffer() ++ currSequence
    var first = true
    while(first || checkIfContain(badSequences, currSequence)) {
       //currSequence.last.explored = false
       currSequence -= currSequence.last
       first = false
       listMoves -= listMoves.last
    }
    currNode = currSequence.last
    currPosition = currNode.position

  }

  def searchSolution() : PacmanSolution =
	{
    currNode.explored = true
    while(! currNode.typeCell.isInstanceOf[Food])
     {
        val goodNeightbours = currNode.neighbours.filter(neightbour => neightbour!= null && !neightbour.explored && !checkIfContain(badSequences, currSequence.+:(neightbour)))
        if(goodNeightbours.isEmpty)
        {
           backtrack()
           //println("Backtrack done")
        }
        else
        {
          val nextNode = goodNeightbours.minBy(neightbour => h(neightbour))
          currNode = nextNode
          currPosition = (currNode.position._1, currNode.position._2)
          currSequence += currNode
          listMoves += currPosition
          currNode.explored = true
          //println("Explored Node: "+currNode)
        }

     }
     return new PacmanSolution(pacmanData.startingPacmanPosition, pacmanData.grid, currSequence.size-1, listMoves.toList)
	}

}