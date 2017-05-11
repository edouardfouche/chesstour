package chesstour

import java.io._
import javax.imageio.ImageIO

/**
  * Created by fouchee on 22.04.17.
  */

/** Abstraction for a chessboard. A chessboard basically consists in a 2-D Array of booleans.
  *
  * In the 2-D Array, false means that the square was not visited yet, and true means that it was visited once
  *
  * @param state The state of the chessboard as a 2-D Array of booleans
  */
class ChessBoard(val state: Array[Array[Boolean]]) extends Serializable{
  /** Alternative constructor, if no parameter is given, a standard empty 8x8 board is created */
  def this() = {
    this(Array.ofDim[Boolean](8,8))
  }
  /** Alternative constructor, construct a rectangular board with custom dimensions
    *
    * @param x The number of square on the x-axis (the origin is placed on the top-left corner, so it corresponds to the y direction).
    * @param y The number of square on the y-axis (the origin is placed on the top-left corner, so it corresponds to the x direction).
    */
  def this(x: Int, y: Int) = {
    this(Array.ofDim[Boolean](x, y))
  }

  val size: (Int,Int) = (state.length, state(0).length)

  /** Get a valid position to start the tour
    * @return a tuple corresponding to a valid x and y position
    * */
  def get_random_start(): (Int,Int) = {
    val x_start = scala.util.Random.nextInt(state.length-1)
    val y_start = scala.util.Random.nextInt(state(0).length-1)
    // TODO: One should make sure at least one move can be done from the starting position
    //val validMoves = moves filterKeys (x => isValidMove((x_start, y_start), board, x))
    //if(!board(x_start)(y_start) & !validMoves.isEmpty) (x_start,y_start)
    if(!state(x_start)(y_start)) (x_start,y_start)
    else get_random_start()
  }

  /** Print the actual board as string, where 1 stands for true in the 2-D Array of booleans and 0 stands for false */
  def show(): Unit = {
    // Initialize a 2-D array of the size of the board, filled  with zeros
    val printed_board = Array.ofDim[Int](state.length,state(0).length)

    for {
      x <- state.indices
      y <- state(x).indices
      if state(x)(y)
    } yield printed_board(x)(y) = 1

    // Print the result as a string
    val result = printed_board
    val ndigit = 1
    result foreach{
      a => a foreach {
        b => print(("%0" + ndigit.toString +"d").format(b) + " | ")
      }; print('\n' + "-"*((3+ndigit)*state.length-1) + '\n')
    }
  }

  /** Clone the current object by transmitting a copy of the 2-D Array of boolean */
  override def clone(): ChessBoard = {
    new ChessBoard(state)
  }

  /** Print basic information about the board, such as its size, the number of true (1) and false (0) in its state */
  override def toString: String = {
    val ncase = state.map(p => p.count(_ == false)) sum
    val string = "size:(" + state.length + "," + state(0).length + "),0:" + ncase + ",1:" + (state.length*state(0).length - ncase)
    string
  }
}

object ChessBoard {
  /** Load a Chessboard from a given image file (as path)
    * @param path Path to the image in the current file system
    * @param switch if true, then the white/transparent pixels in the image will be considered as unvisited, otherwise
    * this is the opposite. This parameter is useful to use the same image as a negative */
  def fromMask(path: String = "knight/heart_30.png", switch:Boolean = false): ChessBoard = {
    val image = ImageIO.read(new File(path))

    val b = Array.ofDim[Boolean](image.getWidth,image.getHeight)

    for{
      x <- 0 until image.getWidth
      y <- 0 until image.getHeight
    } yield {
      if(image.getRGB(x, y) < -1) b(x)(y) = true
      else b(x)(y) = false
    }

    if(!switch) {
      for{
        x <- 0 until image.getWidth
        y <- 0 until image.getHeight
      } yield {
        if(b(x)(y)) {
          b(x)(y) = false
        } else {
          b(x)(y) = true
        }
      }
    }

    new ChessBoard(b)
  }
}
