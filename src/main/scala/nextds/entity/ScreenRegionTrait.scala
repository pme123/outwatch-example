package nextds.entity

case class ScreenRegion(fromLeft: Int = 0, fromTop: Int = 0, width: Int = 1920, height: Int = 1080)

object ScreenRegion {

  val fullHD = ScreenRegion()
  val ultraHD4K = ScreenRegion(width = 3840, height = 2160)
}



