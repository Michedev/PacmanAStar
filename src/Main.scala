import scala.io.StdIn
/**
	* Created by mikedev on 29/08/16.
	*/
object Main
{
	def main(args: Array[String]) {
		val firstTwoLines = StdIn.readLine() +"\n"+ StdIn.readLine()
    val thirdLine = StdIn.readLine()
    var gridString = ""
    val numberRows = thirdLine.substring(0,2).toInt
    for(i <- 0 until numberRows)
    {
      gridString += StdIn.readLine() + (if(i == numberRows - 1) "" else  "\n")
    }
    val input = firstTwoLines + "\n" + thirdLine + "\n" + gridString
		val parser = new InputParser()
		val dataPacman = parser.parseInput(input)
		val pacman = new PacmanAStar(dataPacman)
		val solution = pacman.searchSolution()
		print(solution.cost)
		println(solution.moves.map(pair => "\n"+pair._1+" "+pair._2).reduce((a,b) => a+b))
	}
}
