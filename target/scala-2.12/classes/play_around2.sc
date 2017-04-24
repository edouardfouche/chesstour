import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

import ChessTour._

val image = ImageIO.read(new File("knight/pi2_50.jpg"))


image.getRGB(0, 0)

image.getRGB(10, 25)