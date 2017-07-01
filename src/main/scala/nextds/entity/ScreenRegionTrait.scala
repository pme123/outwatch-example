package nextds.entity

case class ScreenRegion(fromLeft: Int = 0, fromTop: Int = 0, width: Int = 1920, height: Int = 1080) {

  def print(): String = s"x: $fromLeft, y: $fromTop, w: $width, h: $height"

}

object ScreenRegion {

  val fullHD = ScreenRegion()
  val ultraHD4K = ScreenRegion(width = 3840, height = 2160)
}



