package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteTemplTrait
  extends SiteEntityTrait {
  def siteInfo: SiteEntityInfo

  lazy val levelType: LevelType = TEMPL
}

object SiteTemplTrait {
}

case class PlayerTempl(siteInfo: SiteEntityInfo)
  extends SiteTemplTrait
    with PlayerTrait {
}

object PlayerTempl {

  def apply(siteIdent: String, title: String): PlayerTempl = PlayerTempl(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title))
}

case class LayoutTempl(siteInfo: SiteEntityInfo
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

  def singleLayout(siteIdent: String
                   , title: String
                   , screenRegion: ScreenRegion): LayoutTempl =
    LayoutTempl(SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title)
      , screenRegion)
  def singleLayout(templContent: SiteEntityInfo
                   , screenRegion: ScreenRegion): LayoutTempl =
    LayoutTempl(templContent
      , screenRegion)

}

case class PlaylistTempl(siteInfo: SiteEntityInfo)
  extends SiteTemplTrait
    with PlaylistTrait {
}

object PlaylistTempl {
}

case class MediumTempl(siteInfo: SiteEntityInfo)
  extends SiteTemplTrait
    with MediumTrait {
}

object MediumTempl {
}





























