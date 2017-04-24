import ChessTour._
import java.awt.Color

val folder = "knight/"
//val b = ChessBoard.fromMask("knight/knight/knight7_50.png", switch = true)
val b = new ChessBoard((24,16))

val k = new KnightTour(b)


val t = k.warnsdorfsheuristic(random_tie_break=true, closed=true, n_trial = 1000)

t.toString

t.draw(path=folder)
t.simulate(cellsize = 50, nailsize = 15, path=folder, close=false, color=Color.RED)

t.save(path=folder)


//t.show()

//b.show()


//val t = k.warnsdorfsheuristic()

//t.show()