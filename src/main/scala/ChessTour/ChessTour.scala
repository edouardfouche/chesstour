package ChessTour

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.awt.geom.{Ellipse2D, Line2D}
import java.awt.{BasicStroke, Color, Graphics2D}
import java.awt.image.RenderedImage

/**
  * Created by fouchee on 22.04.17.
  */

@SerialVersionUID(114L)
trait ChessTour extends Serializable {
  val moves: Moves
  val board: ChessBoard
  val tour: Tour

  def length(): Int = tour._2.length

  def get_last_pos(): (Int,Int) = {
    //if(movelist.isEmpty) pos
    //else get_last_pos((pos._1 + moves(movelist.head)._1, pos._2 + moves(movelist.head)._2), movelist.tail)
    val pos = tour._1
    val movelist = tour._2

    if(movelist.isEmpty) pos
    else {
      val s = movelist map moves
      //println(s)
      val t = s reduce((a,b) => (a._1+b._1, a._2+b._2))
      (pos._1 + t._1, pos._2 + t._2)
    }
  }


  def isValidMove(pos:(Int,Int), board_state: Array[Array[Boolean]], move:String): Boolean = {
    def newposx = pos._1 + moves(move)._1
    def newposy = pos._2 + moves(move)._2
    if((0 <= newposx) & (newposx < board_state.length) & (0 <= newposy) & (newposy < board_state(0).length)) {
      !board_state(newposx)(newposy)
    } else false
  }

  def isClosed(): Boolean = {
    val endpos = this.get_last_pos()
    val nextMoves = moves.values map {case (val1, val2) => (endpos._1+val1, endpos._2+val2)}
    nextMoves.toSeq.contains(tour._1)
  }

  def isComplete(): Boolean = {
    (tour._2.length+1) == (board.state.length*board.state(0).length) - (board.state map (x => x count (_ == true)) sum)
  }

  def show(): Unit = {
    // Initialize a 2-D array of the size of the board, filled  with zeros
    def printed_board = Array.ofDim[Int](board.state.length,board.state(0).length)

    // Fill the board with numbers, where the number correspond to the order each cases are visited during the tour
    def fillboard(pos:(Int,Int), board:Array[Array[Int]], steps: List[String], acc:Int): Array[Array[Int]] = {
      board(pos._1)(pos._2) = acc
      if(steps.isEmpty) board
      else {
        fillboard((pos._1 + moves(steps.head)._1,pos._2 + moves(steps.head)._2), board, steps.tail, acc+1)
      }
    }

    // Print the result as a string
    val result = fillboard(tour._1, printed_board , tour._2, 1)
    val ndigit = (board.state.length * board.state(0).length).toString.length
    result foreach{
      a => a foreach {
        b => print(("%0" + ndigit.toString +"d").format(b) + " | ")
      }; print('\n' + "-"*((3+ndigit)*board.state.length-1) + '\n')
    }
  }

  // Draw the result on a canvas with all informations
  def draw(cellsize: Int = 50, path: String): Boolean = {
    // I Bootstrapped the code from http://otfried.org/scala/drawing.html
    val circlesize = cellsize * (3.0/5.0)
    val size = (board.size._1*cellsize, board.size._2*cellsize)
    val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
    val g: Graphics2D = canvas.createGraphics()

    // Create the canvas and draw the chess mash
    initialize_drawing(g, size, cellsize)
    // draw filled circle at start
    draw_circle(g, tour._1, cellsize, circlesize.toInt)

    drawboard(g, tour._1, cellsize, circlesize.toInt, tour._2, color=Color.BLUE)

    draw_circle(g, get_last_pos(), cellsize, circlesize.toInt, filled=false, color=Color.red)

    // write image to a file
    output_png(canvas, prefix="drawing", path)
  }

  // custom version of the draw function to simulate the result of the artwork (you can pick colors, sizes, etc)
  def simulate(cellsize:Int = 50, nailsize:Int = 4, path:String, close: Boolean=false, color:Color=Color.black, line_width:Float=4f): Boolean = {
    val circlesize = cellsize * (1.0/5.0)
    val size = (board.size._1*cellsize, board.size._2*cellsize)
    val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
    val g = canvas.createGraphics()

    // Create the canvas and draw the chess mash
    initialize_drawing(g, size, cellsize, draw_mash = false)
    // draw filled circle at start

    drawboard(g, tour._1, cellsize, circlesize.toInt, tour._2, color=color, line_width=line_width)

    if(close) draw_line(g, tour._1, get_last_pos(), cellsize, color, line_width=line_width)

    //if(nail_onway) drawnails_onway(g, tour._1, cellsize, nailsize, tour._2, color=color)
    //else draw_nails(g, size, cellsize, nailsize)
    drawnails_onway(g, tour._1, cellsize, nailsize, tour._2, color=color)

    // write image to a file
    output_png(canvas, prefix="simulation", path)
  }

  // TODO: Save and read from an agnostic and serializable format (like a tuple)
  // Hint: use .getClass.getName
  def save(path:String): Boolean = {
    val toStore = (this.getClass.getName, board, tour)
    val oos = new ObjectOutputStream(new FileOutputStream(path + math.abs(toStore.hashCode).toString))
    oos.writeObject(toStore)
    oos.close
    true
  }


  ///////////////////////////////////
  //// Private printing methods /////
  ///////////////////////////////////

  private def output_png(canvas: RenderedImage, prefix:String = "output", folder: String): Boolean = {
    val filename = folder + prefix + "_" + board.size.toString + "_" + tour._1.toString + "_" + math.abs(tour.hashCode).toString + ".png"
    println(filename)
    javax.imageio.ImageIO.write(canvas, "png", new java.io.File(filename))
  }

  private def drawboard(g: Graphics2D, pos: (Int,Int), cellsize: Int, circlesize: Int,  movelist: List[String], color: Color = Color.BLUE, line_width:Float=4f): Unit = {
    if(movelist.nonEmpty) {
      draw_move(g, pos, moves(movelist.head), cellsize, color=color, line_width = line_width)
      drawboard(g, (pos._1 + moves(movelist.head)._1,pos._2 + moves(movelist.head)._2), cellsize, circlesize, movelist.tail, color, line_width=line_width)
    }
  }

  private def draw_nails(g: Graphics2D, size:(Int,Int), cellsize: Int, nailsize:Int,  color: Color = Color.BLACK) = {
    g.setColor(color)
    def getcenter: Int => Int = x => (x * cellsize + (x + 1) * cellsize) / 2 - nailsize / 2
    for {
      x <- 0 until size._1
      y <- 0 until size._2
    } yield g.fill(new Ellipse2D.Double(getcenter(x), getcenter(y), nailsize, nailsize))
  }

  private def drawnails_onway(g: Graphics2D, pos: (Int,Int), cellsize: Int, nailsize: Int,  movelist: List[String], color: Color = Color.BLUE): Unit = {
    def getcenter: Int => Int = x => (x * cellsize + (x + 1) * cellsize) / 2 - nailsize / 2
    if(movelist.nonEmpty) {
      g.fill(new Ellipse2D.Double(getcenter(pos._1), getcenter(pos._2), nailsize, nailsize))
      drawnails_onway(g, (pos._1 + moves(movelist.head)._1,pos._2 + moves(movelist.head)._2), cellsize, nailsize, movelist.tail, color)
    } else g.fill(new Ellipse2D.Double(getcenter(pos._1), getcenter(pos._2), nailsize, nailsize))
  }

  private def initialize_drawing(g: Graphics2D, size:(Int,Int), cellsize: Int, draw_mash: Boolean = true) = {
    // clear background
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size._1, size._2)
    // enable anti-aliased rendering (prettier lines and circles)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

    if(draw_mash) {
      // Draw the mash of the chessboard
      for{x <- 0 to size._1}{
        g.setStroke(new BasicStroke())
        g.setColor(Color.BLACK)
        g.draw(new Line2D.Double(0, x*cellsize, size._1*cellsize, x*cellsize))
      }
      for{y <- 0 to size._2}{
        g.setStroke(new BasicStroke())
        g.setColor(Color.BLACK)
        g.draw(new Line2D.Double(y*cellsize, 0, y*cellsize, size._2*cellsize))
      }
    }
  }

  // Draw a line on a canvas from a specific position of a specific move w.r.t a specific cell size with a specific color
  private def draw_move(g: Graphics2D, pos: (Int,Int), move: (Int,Int), cellsize:Int, color: Color = Color.BLUE, line_width:Float) = {
    val x_start = pos._1 * cellsize + cellsize/2
    val y_start = pos._2 * cellsize + cellsize/2
    val x_end = x_start + move._1 * cellsize
    val y_end = y_start + move._2 * cellsize
    g.setStroke(new BasicStroke(line_width))
    g.setColor(color)
    g.draw(new Line2D.Double(x_start, y_start, x_end, y_end))
  }

  private def draw_line(g: Graphics2D, startpos: (Int,Int), endpos: (Int,Int), cellsize:Int, color: Color = Color.BLUE, line_width:Float) = {
    val x_start = startpos._1 * cellsize + cellsize/2
    val y_start = startpos._2 * cellsize + cellsize/2
    val x_end = endpos._1 * cellsize + cellsize/2
    val y_end = endpos._2 * cellsize + cellsize/2
    g.setStroke(new BasicStroke(line_width))
    g.setColor(color)
    g.draw(new Line2D.Double(x_start, y_start, x_end, y_end))
  }

  private def draw_circle(g: Graphics2D, pos: (Int,Int), cellsize: Int, circlesize: Int,
                  filled: Boolean = true, color: Color = Color.RED) = {
    g.setColor(color)
    g.setStroke(new BasicStroke(3f))
    val x_circlepos = (pos._1 * cellsize + (pos._1+1) * cellsize)/2 - circlesize/2
    val y_circlepos = (pos._2 * cellsize + (pos._2+1) * cellsize)/2 - circlesize/2
    if(filled) g.fill(new Ellipse2D.Double(x_circlepos, y_circlepos, circlesize, circlesize))
    else g.draw(new Ellipse2D.Double(x_circlepos, y_circlepos, circlesize, circlesize))
  }
}
