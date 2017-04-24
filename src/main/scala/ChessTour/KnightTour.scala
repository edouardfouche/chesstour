package ChessTour
import scala.annotation.tailrec

/**
  * Created by fouchee on 22.04.17.
  */
class KnightTour(val board: ChessBoard, val tour: Tour) extends ChessTour{
  def this() = {
    this(new ChessBoard, ((new ChessBoard).get_random_start(),List()))
  }
  def this(board: ChessBoard) = {
    this(board, (board.get_random_start(),List()))
  }
  // Define the possible moves of a Knight
  val moves = Map("UL" -> (-1,-2), "UR" -> (1,-2), "RU" -> (2,-1), "RD" -> (2,1),
                  "DL" -> (-1,2), "DR" -> (1,2), "LU" -> (-2,-1), "LD" -> (-2,1))

  def warnsdorfsheuristic(random_tie_break: Boolean = false,
                          complete: Boolean = false,
                          closed: Boolean = false,
                          n_trial: Int = 1000): KnightTour = {
    @tailrec def warndorf_acc(pos:(Int,Int), board_state: Array[Array[Boolean]], acc: List[String]): List[String] = {
      board_state(pos._1)(pos._2) = true
      val validMoves = moves filterKeys (x => isValidMove(pos, board_state, x))

      if(validMoves.isEmpty) acc
      else {
        def validMovesDegree = validMoves map {
          case (x, y) => (x, (moves filterKeys (m => isValidMove((pos._1 + y._1, pos._2 + y._2), board_state, m))).size)
        }
        if(!random_tie_break){
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
    var i = n_trial

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

  override def toString: String = {board.toString + "\n" +
                                  "start:(" + tour._1._1 + "," + tour._1._2 + "), length:" + this.length + "\n" +
                                  "closed:" + this.isClosed() + ", complete:" + this.isComplete()}

}


