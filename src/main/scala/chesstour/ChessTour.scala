package chesstour

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.awt.geom.{Ellipse2D, Line2D}
import java.awt.{BasicStroke, Color, Graphics2D}
import java.awt.image.RenderedImage

/**
  * Created by fouchee on 22.04.17.
  */

/** Define general values and methods that a ChessTour should implement. */
trait ChessTour {
  val moves: Moves // A map from String to (Int, Int) that defines the valid moves for this figure (see package object chesstour)
  val board: ChessBoard // The chessboard on which the tour should be found (see ChessBoard documentation)
  val tour: Tour // A tour is a description of the Tour, it contains the starting position and a list of valid moves (see package object chesstour)

  /** The number of moves in the tour */
  def length(): Int = tour._2.length

  /** Iterate over the list of moves given a specific starting position to get the final position of the figure  */
  def get_last_pos(): (Int,Int) = {
    val pos = tour._1
    val movelist = tour._2

    if(movelist.isEmpty) pos
    else {
      val s = movelist map moves
      val t = s reduce((a,b) => (a._1+b._1, a._2+b._2))
      (pos._1 + t._1, pos._2 + t._2)
    }
  }

  /** Determine of a particular move, given a particular position and the state of the board is a valid move
    * @param pos Coordinate of the actual position (x,y).
    * @param board_state A 2-D Array of Boolean that represent the current state of the board.
    * @param move The string corresponding to the move one wants to make.
    * @return Whether the move is valid or not
    * */
  def isValidMove(pos:(Int,Int), board_state: Array[Array[Boolean]], move:String): Boolean = {
    def newposx = pos._1 + moves(move)._1
    def newposy = pos._2 + moves(move)._2
    if((0 <= newposx) & (newposx < board_state.length) & (0 <= newposy) & (newposy < board_state(0).length)) {
      !board_state(newposx)(newposy)
    } else false
  }

  /** Determine if the tour is closed or not
    *
    * @return Whether the tour is closed or not
    */
  def isClosed(): Boolean = {
    val endpos = this.get_last_pos()
    val nextMoves = moves.values map {case (val1, val2) => (endpos._1+val1, endpos._2+val2)}
    nextMoves.toSeq.contains(tour._1)
  }

  /** Determine if the tour is complete or not, i.e., if it actually visited all the squares
    *
    * @return Whether the tour is complete or not
    */
  def isComplete(): Boolean = {
    (tour._2.length+1) == (board.state.length*board.state(0).length) - (board.state map (x => x count (_ == true)) sum)
  }

  /** A basic method that print as a string the order of the path (as numbers) in a 2-D matrix */
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

  /** Draw the current tour on a canvas and save it in a .png file
    * @param cellSize Number of pixel for each square, determine the size of the file that will be written.
    * @param path Path to a folder where the file should be written.
    * @return Whether the operation succeed or not.
    * */
  def draw(cellSize: Int = 50, path: String): Boolean = {
    // I Bootstrapped the code from http://otfried.org/scala/drawing.html
    val circlesize = cellSize * (3.0/5.0)
    val size = (board.size._1*cellSize, board.size._2*cellSize)
    val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
    val g: Graphics2D = canvas.createGraphics()

    // Create the canvas and draw the chess mesh
    initialize_drawing(g, size, cellSize)
    // draw filled circle at start
    draw_circle(g, tour._1, cellSize, circlesize.toInt)

    drawboard(g, tour._1, cellSize, circlesize.toInt, tour._2, color=Color.BLUE)

    draw_circle(g, get_last_pos(), cellSize, circlesize.toInt, filled=false, color=Color.red)

    // write image to a file
    output_png(canvas, prefix="drawing", path)
  }

  // custom version of the draw function to simulate the result of the artwork (you can pick colors, sizes, etc)
  /** Custom version of the draw method, where one has more control on the aesthetic of the output.
    * @param cellSize Number of pixel for each square, determine the size of the file that will be written.
    * @param nailSize A the center of each cell, a circle is drawn to symbolize nails in the final real world artworks.
    *                 This parameter determine the size in pixel of those nails.
    * @param lineWidth Width of the line of the path in pixel .
    * @param closed Wheter the tour should be closed or not, i.e., if a segment should be drawn between the start and end points .
    * @param color Color of the tour path.
    * @param path Path to a folder where the file should be written.
    * */
  def simulate(cellSize:Int = 50, nailSize:Int = 4, linewidth:Float=4f, closed: Boolean=false, color:Color=Color.black, path:String): Boolean = {
    val circlesize = cellSize * (1.0/5.0)
    val size = (board.size._1*cellSize, board.size._2*cellSize)
    val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
    val g = canvas.createGraphics()

    // Create the canvas and draw the chess mesh
    initialize_drawing(g, size, cellSize, draw_mesh = false)
    // draw filled circle at start

    drawboard(g, tour._1, cellSize, circlesize.toInt, tour._2, color=color, linewidth=linewidth)

    if(closed) draw_line(g, tour._1, get_last_pos(), cellSize, color, linewidth=linewidth)

    //if(nail_onway) drawnails_onway(g, tour._1, cellsize, nailsize, tour._2, color=color)
    //else draw_nails(g, size, cellsize, nailsize)
    drawnails_onway(g, tour._1, cellSize, nailSize, tour._2, color=color)

    // write image to a file
    output_png(canvas, prefix="simulation", path)
  }

  // TODO: Save and read from an agnostic and serializable format (like a tuple)
  // Hint: use .getClass.getName
  /**
    * Save the current tour to a file
    * @param path Path to a folder where the file should be written.
    * @return Whether the operation succeed or not.
    */
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

  /** Output the current canvas as a png with a custom prefix in a folder */
  private def output_png(canvas: RenderedImage, prefix:String = "output", folder: String): Boolean = {
    val filename = folder + prefix + "_" + board.size.toString + "_" + tour._1.toString + "_" + math.abs((this.getClass.getName, board, tour).hashCode).toString + ".png"
    println(filename)
    javax.imageio.ImageIO.write(canvas, "png", new java.io.File(filename))
  }

  /** Draw the path on the canvas from a specific list of moves */
  private def drawboard(g: Graphics2D, pos: (Int,Int), cellsize: Int, circlesize: Int,  movelist: List[String], color: Color = Color.BLUE, linewidth:Float=4f): Unit = {
    if(movelist.nonEmpty) {
      draw_move(g, pos, moves(movelist.head), cellsize, color=color, linewidth = linewidth)
      drawboard(g, (pos._1 + moves(movelist.head)._1,pos._2 + moves(movelist.head)._2), cellsize, circlesize, movelist.tail, color, linewidth=linewidth)
    }
  }

  /** Draw all the nails on the board, even if the square what not visited */
  private def draw_nails(g: Graphics2D, size:(Int,Int), cellsize: Int, nailsize:Int,  color: Color = Color.BLACK) = {
    g.setColor(color)
    def getcenter: Int => Int = x => (x * cellsize + (x + 1) * cellsize) / 2 - nailsize / 2
    for {
      x <- 0 until size._1
      y <- 0 until size._2
    } yield g.fill(new Ellipse2D.Double(getcenter(x), getcenter(y), nailsize, nailsize))
  }

  /** Draw the nails only for squares that were visited */
  private def drawnails_onway(g: Graphics2D, pos: (Int,Int), cellsize: Int, nailsize: Int,  movelist: List[String], color: Color = Color.BLUE): Unit = {
    def getcenter: Int => Int = x => (x * cellsize + (x + 1) * cellsize) / 2 - nailsize / 2
    if(movelist.nonEmpty) {
      g.fill(new Ellipse2D.Double(getcenter(pos._1), getcenter(pos._2), nailsize, nailsize))
      drawnails_onway(g, (pos._1 + moves(movelist.head)._1,pos._2 + moves(movelist.head)._2), cellsize, nailsize, movelist.tail, color)
    } else g.fill(new Ellipse2D.Double(getcenter(pos._1), getcenter(pos._2), nailsize, nailsize))
  }

  /** Initialize the canvas and optionally draw the chess mesh on it */
  private def initialize_drawing(g: Graphics2D,size:(Int,Int), cellsize: Int, draw_mesh: Boolean = true) = {
    // clear background
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size._1, size._2)
    // enable anti-aliased rendering (prettier lines and circles)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

    if(draw_mesh) {
      // Draw the mesh of the chessboard
      for{x <- 0 to size._1/cellsize}{
        g.setStroke(new BasicStroke())
        g.setColor(Color.BLACK)
        if(x == size._1/cellsize) g.draw(new Line2D.Double(0, x*cellsize-1, size._1*cellsize, x*cellsize-1))
        else g.draw(new Line2D.Double(0, x*cellsize, size._1*cellsize, x*cellsize))
      }
      for{y <- 0 to size._2/cellsize}{
        g.setStroke(new BasicStroke())
        g.setColor(Color.BLACK)
        if(y == size._2/cellsize) g.draw(new Line2D.Double(y*cellsize-1, 0, y*cellsize-1, size._2*cellsize))
        else  g.draw(new Line2D.Double(y*cellsize, 0, y*cellsize, size._2*cellsize))
      }
    }
  }

  /** Draw a line on a canvas from a specific position of a specific move w.r.t a specific cell size with a specific color */
  private def draw_move(g: Graphics2D, pos: (Int,Int), move: (Int,Int), cellsize:Int, color: Color = Color.BLUE, linewidth:Float) = {
    val x_start = pos._1 * cellsize + cellsize/2
    val y_start = pos._2 * cellsize + cellsize/2
    val x_end = x_start + move._1 * cellsize
    val y_end = y_start + move._2 * cellsize
    g.setStroke(new BasicStroke(linewidth))
    g.setColor(color)
    g.draw(new Line2D.Double(x_start, y_start, x_end, y_end))
  }

  /** Draw a line between to positions in the canvas */
  private def draw_line(g: Graphics2D, startpos: (Int,Int), endpos: (Int,Int), cellsize:Int, color: Color = Color.BLUE, linewidth:Float) = {
    val x_start = startpos._1 * cellsize + cellsize/2
    val y_start = startpos._2 * cellsize + cellsize/2
    val x_end = endpos._1 * cellsize + cellsize/2
    val y_end = endpos._2 * cellsize + cellsize/2
    g.setStroke(new BasicStroke(linewidth))
    g.setColor(color)
    g.draw(new Line2D.Double(x_start, y_start, x_end, y_end))
  }

  /** Draw a circle at a particular position in the canvas */
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
