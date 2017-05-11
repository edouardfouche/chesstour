package chesstour
import scala.annotation.tailrec

/**
  * Created by fouchee on 22.04.17.
  */

/** Abstraction for the tour a Knight on a chessboard. It consists essentially in board of class ChessBoard and a
  * tour of Type Tour (see package object chesstour)
  *
  * @param board an object of type ChessBoard, on which the tour should take place
  * @param tour a value of Type Tour, that describes a knight tour (starting position and list of valid moves)
  */
class KnightTour(val board: ChessBoard, val tour: Tour) extends ChessTour{
  /** Alternative constructor, a KnightTour is instantiated on a standard chessboard (8x8) and the tour initialized at
    * a random starting position with an empty list of moves */
  def this() = {
    this(new ChessBoard, ((new ChessBoard).get_random_start(),List()))
  }
  /** Alternative constructor, a KnightTour is instantiated on the given board and the tour initialized at a random
    * starting position with an empty list of moves
    * @param board an object of class Chessboard where the tour should be found. */
  def this(board: ChessBoard) = {
    this(board, (board.get_random_start(),List()))
  }
  // Define the possible moves of a Knight
  val moves = Map("UL" -> (-1,-2), "UR" -> (1,-2), "RU" -> (2,-1), "RD" -> (2,1),
                  "DL" -> (-1,2), "DR" -> (1,2), "LU" -> (-2,-1), "LD" -> (-2,1))

  /** Find a tour using Warnsdorf's heuristic. More information about can be found here edouardfouche.com/Digital-Art-&-the-Knight's-Tour-Problem/
    * @param randomTieBreak Whether to solve ties by picking a move at random or not. If not, the algorithm pick the move
    * whose string is the lowest in lexicographical order among the tying moves.
    * @param complete Whether we are interested in finding a complete tour. Complete tour are tour that visit all free
    *                 squares of the board. Such tour might not exists. In this case the method will return the tour with
    *                 the highest number of visited squares among the n trials.
    * @param closed Whether we are interesting only in closed tour. A tour is closed is the starting square is reachable
    *               from the ending square. Such tour might be difficult to find, especially if the tour is required to
    *               be complete. The method will perform n trial and return the first closed tour it found, if complete=true
    *               it will return the closed tour will the highest number of visited squares among the n trials.
    * @param n Number of trials the method may perform before returning its result.
    * */
  def warnsdorfsHeuristic(randomTieBreak: Boolean = true,
                          complete: Boolean = false,
                          closed: Boolean = false,
                          n: Int = 1000): KnightTour = {
    @tailrec def warndorf_acc(pos:(Int,Int), board_state: Array[Array[Boolean]], acc: List[String]): List[String] = {
      board_state(pos._1)(pos._2) = true
      val validMoves = moves filterKeys (x => isValidMove(pos, board_state, x))

      if(validMoves.isEmpty) acc
      else {
        def validMovesDegree = validMoves map {
          case (x, y) => (x, (moves filterKeys (m => isValidMove((pos._1 + y._1, pos._2 + y._2), board_state, m))).size)
        }
        if(!randomTieBreak){
          val nextMove =  validMovesDegree.minBy(x => x._2)
          warndorf_acc((pos._1 + moves(nextMove._1)._1, pos._2 + moves(nextMove._1)._2), board_state, acc ::: List(nextMove._1))
        }
        else {
          val possibleMoves =  validMovesDegree filter (x => x._2 == validMovesDegree.minBy(x => x._2)._2) toArray
          val nextMove = possibleMoves(scala.util.Random.nextInt(possibleMoves.length))
          warndorf_acc((pos._1 + moves(nextMove._1)._1, pos._2 + moves(nextMove._1)._2), board_state, acc ::: List(nextMove._1))
        }
      }
    }

    // I agree that this part is rather imperative, but this avoids running into impossible tail recursion problems
    // during hill climbing, because that would be a double tail recusion (warndorf_acc is already tail recursive)
    var result = new KnightTour(board, (tour._1, warndorf_acc(tour._1, board.state.map(_.clone), List())))
    var i = n

    if(closed) {
      if(!result.isClosed) {
        while(i != 0 & !result.isClosed()) {
          //println("Current: " + this.length + ", start:" + this.tour._1.toString())
          val random_start = board.get_random_start()
          val other_result = new KnightTour(board, (random_start, warndorf_acc(random_start, board.state.map(_.clone), List())))
          if(other_result.isClosed()) {
            result = other_result
          }
          i -= 1
        }
      } else {
        while(i != 0 & !result.isComplete()) {
          //println("Current: " + this.length + ", start:" + this.tour._1.toString())
          val random_start = board.get_random_start()
          val other_result = new KnightTour(board, (random_start, warndorf_acc(random_start, board.state.map(_.clone), List())))

          if(other_result.length > result.length & other_result.isClosed()) {
            result = other_result
          }
          i -= 1
        }
      }
    }

    if(complete) {
      if (!result.isComplete) {
        while (i != 0 & !result.isComplete()) {
          //println("Current: " + this.length + ", start:" + this.tour._1.toString())
          val random_start = board.get_random_start()
          val other_result = new KnightTour(board, (random_start, warndorf_acc(random_start, board.state.map(_.clone), List())))

          if (other_result.length > result.length) {
            result = other_result
          }
          i -= 1
        }
      }
    }

    if(result.isComplete()) println("The tour is complete.") else println("The tour is not complete.")
    if(result.isClosed()) println("The tour is closed.") else println("The tour is not closed.")
    result
  }

  /** Print basic information about the Knight's Tour, such as its start, its length, if it is closed or not */
  override def toString: String = {board.toString + "\n" +
                                  "start:(" + tour._1._1 + "," + tour._1._2 + "), length:" + this.length + "\n" +
                                  "closed:" + this.isClosed() + ", complete:" + this.isComplete()}

}


