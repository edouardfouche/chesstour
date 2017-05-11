import ChessTour.{ChessBoard, KnightTour}
import java.awt.Color



import ChessTour._
val output_folder = "knight/" // define an output folder

val board = ChessBoard.fromMask("knight/heart_30_3.png", switch=true)
//val board = new ChessBoard((8,8))
val knight = new KnightTour(board)

val tour = knight.warnsdorffsheuristic(random_tie_break=true, complete=true, closed=true, n_trials = 1000)
tour.draw(path=output_folder)

tour.toString

tour.simulate(cellsize = 50, nailsize = 15, path=output_folder, closed=true, color=Color.RED)

tour.save(output_folder)

//t.show()

//b.show()


//val t = k.warnsdorfsheuristic()

//t.show()