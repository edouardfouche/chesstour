import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

/**
  * Created by fouchee on 22.04.17.
  */
package object ChessTour {
  type Tour = ((Int,Int), List[String])
  type Moves = Map[String, (Int,Int)]

  def load(path:String): ChessTour = {
    val ois = new ObjectInputStream(new FileInputStream(path))
    val storedTour = ois.readObject.asInstanceOf[Tuple3[String,ChessBoard,Tour]]

    val chessTour = storedTour match {
      case ("ChessTour.KnightTour",board,tour) => new ChessTour.KnightTour(board, tour)
      case (other,board,tour) => throw new Error(s"Unknown class : $other")
      case _ => throw new Error("Unvalid stored format")
    }
    ois.close
    chessTour
  }
}
