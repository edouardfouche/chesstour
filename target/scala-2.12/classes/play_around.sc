import ChessTour._
import java.awt.Color

val folder = "knight/"
val b = ChessBoard.fromMask("knight/pi2_50.jpg", switch = true)
val b = ChessBoard()
/
val k = new KnightTour(b)


val t = k.warnsdorfsheuristic(random_tie_break=true, complete=true, n_trial = 1000)

t.toString

t.draw(path=folder)
t.simulate(cellsize = 50, nailsize = 15, path=folder, close=false, color=Color.RED)

t.save(path=folder)


//t.show()

//b.show()


//val t = k.warnsdorfsheuristic()

//t.show()