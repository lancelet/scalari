package ri.ssg.rman

import scala.collection.immutable.Seq

import ri.{Context, Ri}
import ri.ssg.RenderSettings

case class RmanRenderSettings(
  imageWidth: Int = 640,
  imageHeight: Int = 360,
  pixelFilter: ri.FilterFunc = ri.MitchellFilter,
  pixelFilterWidth: Double = 2.0,
  pixelFilterHeight: Double = 2.0,
  useFramebuffer: Boolean = true,
  shutterOpen: Double = 0.25,
  shutterClose: Double = 0.75,
  frameDuration: Double = 1.0 / 30.0,
  mBlurSlices: Int = 5
) extends RenderSettings {

  def toRi(context: Context) {
    val r = new Ri()
    r.Resume(context) {
      r.Format(imageWidth, imageHeight, 1)
      r.PixelFilter(pixelFilter, pixelFilterWidth, pixelFilterHeight)
      r.Shutter(shutterOpen, shutterClose)
    } // Resume
  }

  private val shutterTimes: Seq[Double] = for (i <- 0 until mBlurSlices) yield (i.toDouble / (mBlurSlices-1))
  def times(startTime: Double): Seq[Double] = shutterTimes map (_ * frameDuration + startTime)
  def openCloseTimes(startTime: Double): Pair[Double,Double] = (startTime, startTime+frameDuration)
}