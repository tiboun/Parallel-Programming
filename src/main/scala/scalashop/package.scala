import scala.collection.immutable.IndexedSeq

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val startX = clamp(x - radius, 0, src.width - 1)
    val endX = clamp(x + radius, 0, src.width - 1)
    val startY = clamp(y - radius, 0, src.height - 1)
    val endY = clamp(y + radius, 0, src.height - 1)
    val values: IndexedSeq[(Int, Int, Int, Int)] = for {
      i <- (startX to endX)
      j <- (startY to endY)
    } yield (red(src(i, j)), green(src(i, j)), blue(src(i, j)), alpha(src(i, j)))
    val (totalR, totalG, totalB, totalA): (Int, Int, Int, Int) = values.reduceLeft ((c1, c2) => (c1._1 + c2._1, c1._2 + c2._2, c1._3 + c2._3, c1._4 + c2._4))
    val nbElement = (endX - startX + 1) * (endY - startY + 1)
    rgba(totalR / nbElement, totalG / nbElement, totalB / nbElement, totalA / nbElement)
  }

}
