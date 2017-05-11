import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

/**
  * Created by fouchee on 22.04.17.
  */

/** Define type aliases to describe tours and valid moves, as well as the function to load a saved tour */
package object chesstour {
  /** A Tour is described as a 2-Tuple, containing a 2-Tuple of Int (the starting position) and a List of String which
    * contains the name of the different moves that were done successively.
    * */
  type Tour = ((Int,Int), List[String])
  /** The Moves of a Figure are described by a Map from String to (Int, Int), where the String is a name for the move
    * and the (Int, Int) is the offset on the board for this move.
    * For example, a possible move for a Knight is "UL" (Up-Left) which corresponds to the offset (-1,-2)
    * This is not to be confused with "LU" (Left-Up) which corresponds to the offset (-2,-1)
    * */
  type Moves = Map[String, (Int,Int)]

  /** Load a saved tour from a file
    *
    * @param path Path to a saved tour, by default, tour are saved under a hashcode
    * @return The corresponding saved ChessTour object
    * */
  def load(path:String): ChessTour = {
    val ois = new ObjectInputStream(new FileInputStream(path))
    val storedTour = ois.readObject.asInstanceOf[Tuple3[String,ChessBoard,Tour]]

    val chessTour = storedTour match {
      case ("ChessTour.KnightTour",board,tour) => new chesstour.KnightTour(board, tour)
      case (other,board,tour) => throw new Error(s"Unknown class : $other")
      case _ => throw new Error("Unvalid stored format")
    }
    ois.close()
    chessTour
  }
}
