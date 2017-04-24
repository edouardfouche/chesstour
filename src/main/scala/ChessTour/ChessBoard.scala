package ChessTour

import java.io._
import javax.imageio.ImageIO

/**
  * Created by fouchee on 22.04.17.
  */

class ChessBoard(val state: Array[Array[Boolean]]) extends Serializable{
  def this() = {
    this(Array.ofDim[Boolean](8,8))
  }
  def this(dim: (Int, Int)) = {
    this(Array.ofDim[Boolean](dim._1, dim._2))
  }
  val size: (Int,Int) = (state.length, state(0).length)

  def get_random_start(): (Int,Int) = {
    val x_start = scala.util.Random.nextInt(state.length-1)
    val y_start = scala.util.Random.nextInt(state(0).length-1)
    //val validMoves = moves filterKeys (x => isValidMove((x_start, y_start), board, x))
    //if(!board(x_start)(y_start) & !validMoves.isEmpty) (x_start,y_start)
    if(!state(x_start)(y_start)) (x_start,y_start)
    else get_random_start()
  }

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

  override def clone(): ChessBoard = {
    new ChessBoard(state)
  }

  override def toString: String = {
    val ncase = state.map(p => p.count(_ == false)) sum
    val string = "size:(" + state.length + "," + state(0).length + "),0:" + ncase + ",1:" + (state.length*state(0).length - ncase)
    string
  }
}

object ChessBoard {
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

    if(switch) {
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
