package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteTemplTrait
  extends SiteEntityTrait {
  def templContent: SiteTempl

  lazy val levelType: LevelType = TEMPL
  lazy val siteIdent: String = templContent.siteId
  lazy val title: String = templContent.title
  lazy val descr: String = templContent.descr

  lazy val maybeTitle: Option[String] = Some(title)
  lazy val maybeDescr: Option[String] = Some(descr)
}

object SiteTemplTrait {
}

case class SiteTempl(siteId: String, title: String, descr: String = "No description available")

object SiteTempl {
}

case class PlayerTempl(templContent: SiteTempl)
  extends SiteTemplTrait
    with PlayerTrait {
}

object PlayerTempl {

  def apply(siteId: String, title: String): PlayerTempl = PlayerTempl(SiteTempl(siteId, title))
}

case class LayoutTempl(templContent: SiteTempl
                       , screenRegion: ScreenRegion)
  extends SiteTemplTrait
    with LayoutTrait {

  lazy val fromLeft: Int = screenRegion.fromLeft
  lazy val fromTop: Int = screenRegion.fromTop
  lazy val width: Int = screenRegion.width
  lazy val height: Int = screenRegion.height

  lazy val maybeScreenRegion: Option[ScreenRegion] = Some(screenRegion)
}

object LayoutTempl {

  def singleLayout(siteId: String
                   , title: String
                   , screenRegion: ScreenRegion): LayoutTempl =
    LayoutTempl(SiteTempl(siteId, title)
      , screenRegion)

}

case class PlaylistTempl(templContent: SiteTempl)
  extends SiteTemplTrait
    with PlaylistTrait {
}

object PlaylistTempl {
}

case class MediumTempl(templContent: SiteTempl)
  extends SiteTemplTrait
    with MediumTrait {
}

object MediumTempl {
}





























