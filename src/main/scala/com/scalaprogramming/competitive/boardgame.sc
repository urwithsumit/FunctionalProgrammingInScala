
/**
* https://leetcode.com/problems/sliding-puzzle/
*
*/
object Solution {

  def slidingPuzzle(board: Array[Array[Int]]): Int = {

    type Pair = (Int, Int)

    case class BoardSolution(board: String, step: Int, vertex: Pair, name: String)

    val Results = scala.collection.mutable.Set[Int]()

    val queue = scala.collection.mutable.Queue[BoardSolution]()

    def _mkString(arr: Array[Array[Int]]) = arr.map(c => c.mkString(",").trim).mkString(",").trim

    def printBoard(board: Array[Array[Int]]): String = _mkString(board)

    val finalState = _mkString(Array(Array(1, 2, 3), Array(4, 5, 0)))

    val initialState = _mkString(board)

    val visitedStates = scala.collection.mutable.Set[String]()

    def isSolved(board: String): Boolean = board == finalState

    def isBackInitialState(board: String): Boolean = {
      if (visitedStates.nonEmpty) board == initialState
      else false
    }

    /**
    * Prepare list of all available valid moves at the given index..
    *
    */
    def nextAvlMoves(initial: Pair, board: Array[Array[Int]]): List[Pair] = {

      val directions = Array((-1, 0), (1, 0), (0, -1), (0, 1))

      val possibleMoves = directions.foldLeft(List[Pair]()) {
        case (acc, dir) => acc :+ (initial._1 + dir._1, initial._2 + dir._2)
      }

      // Perform array boundary checks on possible moves.
      for {
        move <- possibleMoves
        if (0 <= move._1 && move._1 < board.length
          && 0 <= move._2 && move._2 < board(0).length)
      } yield {
        move
      }
    }

    /**
    * Convert String representation of board to an Array object.
    */
    def toArrBoard(_state: String): Array[Array[Int]] = {
      val state = _state.replaceAll(",", "").replaceAll("\\s+", "")

      def convertToIntArr(str: String): Array[Int] = {
        str.map(ch => ch - '0').toArray
      }

      Array(state.substring(0, state.length / 2), state.substring(state.length / 2, state.length)).map(str => convertToIntArr(str))
    }

    /**
    * Check if the steps are lower than the minimum steps available in Results.
    */
    def smallerSolAvl(steps: Int): Boolean = {
      if (!Results.isEmpty) Results.min < steps else false
    }

    /**
    * BFS
    */
    def _solvePuzzle(): Unit = {

      while (!queue.isEmpty) {
        val qElem = queue.dequeue
        // Convert String Board to an Array Object
        val arrBoard = toArrBoard(qElem.board)

        // Check avl moves i.e. if there is an adjacent index with 0
        val avlMoves = nextAvlMoves(qElem.vertex, arrBoard).filter(c => arrBoard(c._1)(c._2) == 0)

        avlMoves.map {
          pair =>
            // Swap the element vertex with the vertex having 0 value
            val tmp = arrBoard(qElem.vertex._1)(qElem.vertex._2)
            arrBoard(qElem.vertex._1)(qElem.vertex._2) = arrBoard(pair._1)(pair._2) // putting 0 in vertex position
            arrBoard(pair._1)(pair._2) = tmp // pair set to elem value

            // Once swapped, the Array object is the available next board. Let's convert it to a String representation.
            val nextBoard = printBoard(arrBoard)

            //println(s"${qElem.name} Board Changes: ${qElem.board} => ${nextBoard}")

            // Is this board state desired final state, yes, than increment the step and add to Result.
            if (isSolved(nextBoard)) Results.addOne(qElem.step + 1)
            // Is this board state back to initial state i.e. we detected a cycle. let's add to visited and process no further.
            else if (isBackInitialState(nextBoard) && !visitedStates.contains(nextBoard)) visitedStates.addOne(printBoard(arrBoard))
            else {
              // Check if the board state is not already visited.
              // Also, check if the current solution step count is not longer than a solution already found.
              if (!visitedStates.contains(nextBoard) && !smallerSolAvl(qElem.step + 1)) {

                // Add current board to visited states.
                visitedStates.addOne(nextBoard)

                // Vertex is updated to 0, hence we are looking for nextAvlMoves for this vertex 0
                // Next avl moves consists of valid adjacent neighbour of 0, excluding the pair we last swapped.
                nextAvlMoves(qElem.vertex, arrBoard).filterNot(_ == pair).map { _move =>
                  queue.addOne(BoardSolution(nextBoard, qElem.step + 1, _move, s"${qElem.name} -> S-${qElem.step + 1}"))
                }
              }
            }
         }
      }
    }

    /**
    * Initialize search with the index of 0
    */
    def _initialize: Unit = {
      for {
        i <- 0 until board.length
        j <- 0 until board(0).length
        if (board(i)(j) == 0)
      } yield {
        // Find all adjacent vertices to 0.
        val avlMoves = nextAvlMoves((i, j), board)

        var thread = 0
        for {move <- avlMoves}
          yield {
            thread += 1
            queue.addOne(BoardSolution(printBoard(board), 0, move, s"Start -> T-${thread}"))
          }
      }
    }


    if (!isSolved(printBoard(board))) {
      _initialize
      _solvePuzzle
    } else Results.addOne(0)


    //println(s"Result Board: ${printBoard(board)}")
    //println(Results.mkString(","))

    if (Results.isEmpty) -1 else Results.min
  }
}
