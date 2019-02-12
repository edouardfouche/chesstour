import chesstour.{ChessBoard, KnightTour}
import java.awt.Color



import chesstour._
val output_folder = "/home/fouchee/knight/" // define an output folder

//val board = ChessBoard.fromMask("knight/heart_30_3.png")
val board = new ChessBoard(70,18)
val knight = new KnightTour(board)

val tour = knight.warnsdorfsHeuristic(randomTieBreak=true, complete=true, closed=true, n = 100000)
tour.draw(path=output_folder)

tour.toString

tour.simulate(cellSize = 50, nailSize = 15, path=output_folder, closed=true, color=Color.BLACK)

tour.save(output_folder)

//t.show()

//b.show()


//val t = k.warnsdorfsheuristic()

//t.show()