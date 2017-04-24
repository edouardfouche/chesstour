import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

import ChessTour._

val folder = "knight/knight/best/"
val t = ChessTour.load("knight/knight/best/585997308")
//t.simulate(cellsize = 70, nailsize = 0, path=folder, close=false, color=Color.BLACK, line_width = 10f)

/*
val b = new ChessBoard((24,16))
val k = new KnightTour(b)

val cl = k.getClass().getName()


new ChessTour.KnightTour

val a = (1,2,3)

a.getClass