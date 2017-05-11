import chesstour.{ChessBoard, KnightTour}
import java.awt.Color



import chesstour._
val output_folder = "knight/" // define an output folder

val board = ChessBoard.fromMask("knight/heart_30_3.png")
//val board = new ChessBoard(8,8)
val knight = new KnightTour(board)

val tour = knight.warnsdorfsHeuristic(randomTieBreak=true, complete=true, closed=true, n_trials = 1000)
tour.draw(path=output_folder)

tour.toString

tour.simulate(cellSize = 50, nailSize = 15, path=output_folder, closed=true, color=Color.RED)

tour.save(output_folder)

//t.show()

//b.show()


//val t = k.warnsdorfsheuristic()

//t.show()