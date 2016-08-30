import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, MutableList}
/**
  * Created by mikedev on 29/08/16.
  */
class InputParser {

  private def addLinks(i: Int, j: Int, inputList : Array[String], nodeList : MutableList[MutableList[Node]] ) =
  {
      val allowedChar = List('-', 'P', '.')
      val currNode = nodeList(i)(j)
      if(i != 0)
      {
         val char = inputList(i-1)(j)
        if(allowedChar contains char)
        { currNode.topNeightbour = nodeList(i-1)(j); nodeList(i-1)(j).bottomNeightbour = currNode; }

      }
      if(j != 0)
      {
        val char = inputList(i)(j-1)
        if(allowedChar contains char)
        {currNode.leftNeightbour = nodeList(i)(j-1); nodeList(i)(j-1).rightNeightbour = currNode }
      }

  }

  private def parseGrid(lineList: Array[String]): List[List[Node]] =
  {
    val output = MutableList.fill(lineList.length /*first and last line are all "%"*/)(MutableList.fill(lineList.head.length)(new Node()))
    var (i,j) = (0,0)
     for(line <- lineList)
     {
        j = 0
        for(char <- line)
        {
           output(i)(j).position = (i,j)
           char match{
             case '.' => { output(i)(j).typeCell = Food(); addLinks(i, j, lineList, output) }
             case 'P' => { output(i)(j).typeCell = Pacman(); addLinks(i, j, lineList, output) }
             case '%' => Nil
             case '-' => { output(i)(j).typeCell = Floor(); addLinks(i, j, lineList, output) }
           }
          j += 1
        }
       i+=1
     }
    return output.map(_.toList).toList
  }
  
  def parseInput(input: String) : PacmanData =
  {
     val inputByLine : Array[String] = input.split("\n")
    
     val pacmanPosition : Array[String] = inputByLine(0).split(" ")
     val (xPacman, yPacman) = (pacmanPosition(0).toInt, pacmanPosition(1).toInt)
    
    val foodPosition : Array[String] = inputByLine(1).split(" ")
    val (xFood, yFood) = (foodPosition(0).toInt, foodPosition(1).toInt)
  
    val sizeGrid : Array[String] = inputByLine(2).split(" ")
    val (rowSize, colSize) = (sizeGrid(0).toInt, sizeGrid(1).toInt)
    
    return PacmanData(new Pair(xPacman, yPacman), new Pair(xFood, yFood), new Pair(rowSize, colSize), parseGrid(inputByLine.takeRight(inputByLine.length - 3)))
  
  }

}
