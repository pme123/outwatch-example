package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteCompTrait
  extends SiteEntityTrait {

  def siteInfo: SiteComp[_ <: SiteTemplTrait]

  def templ: SiteTemplTrait = siteInfo.templ

  lazy val levelType: LevelType = COMP

}

case class SiteComp[T <: SiteTemplTrait](siteIdent: SiteIdent
                                         , ident: SiteEntityIdent
                                         , templ: T
                                         , maybeTitle: Option[String] = None
                                         , maybeDescr: Option[String] = None)
  extends SiteEntityInfoTrait {
  lazy val title: String = maybeTitle.getOrElse(templ.title)
  lazy val descr: String = maybeDescr.getOrElse(templ.descr)

}

object SiteComp {
  def apply[T <: SiteTemplTrait](templ: T): SiteComp[T] = SiteComp(templ.siteIdent, Site.nextIdent(templ.siteIdent), templ)
  def apply[T <: SiteTemplTrait](siteIdent: String
                                 ,templ: T
                                 , title:String): SiteComp[T] = SiteComp(siteIdent, Site.nextIdent(siteIdent), templ, Some(title))
  def apply[T <: SiteTemplTrait](siteIdent: String
                                 ,templ: T): SiteComp[T] = SiteComp(siteIdent, Site.nextIdent(siteIdent), templ)

}

case class PlayerComp(siteInfo: SiteComp[PlayerTempl]
                      , status: PlayerStatus = PlayerStatus.NOT_CONNECTED
                      , maybeLocation: Option[PlayerLocation] = None)
  extends SiteCompTrait
    with PlayerTrait {
}

object PlayerComp {


  def apply(siteIdent: String
            , templ: PlayerTempl): PlayerComp =
    PlayerComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ))

  def apply(siteIdent: String
            , templ: PlayerTempl
            , title: String): PlayerComp =
    PlayerComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ, Some(title)))

  def apply(siteIdent: String
            , templ: PlayerTempl
            , title: String
            , status: PlayerStatus
            , location: PlayerLocation): PlayerComp =
    PlayerComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ, Some(title))
      , status, Some(location))
}

sealed trait PlayerStatus

object PlayerStatus {

  case object RUNNING extends PlayerStatus

  case object STOPPED extends PlayerStatus

  case object NOT_CONNECTED extends PlayerStatus

}

case class PlayerLocation(lat: Double, lng: Double)

case class LayoutComp(siteInfo: SiteComp[LayoutTempl]
                      , screenRegionOpt: Option[ScreenRegion] = None)
  extends SiteCompTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteInfo.templ.screenRegion)
  lazy val maybeScreenRegion: Option[ScreenRegion] = screenRegionOpt

}

object LayoutComp {

  def apply(templ: LayoutTempl): LayoutComp =
    LayoutComp(SiteComp(templ))

  def apply(siteIdent: String
            , templ: LayoutTempl): LayoutComp =
    LayoutComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ))

  def apply(siteIdent: String
            , templ: LayoutTempl
            , title: String
            , screenRegion: ScreenRegion): LayoutComp =
    LayoutComp(SiteComp(siteIdent
      , Site.nextIdent(siteIdent)
      , templ
      , Some(title))
      , Some(screenRegion))

}

case class PlaylistComp(siteInfo: SiteComp[PlaylistTempl])
  extends SiteCompTrait
    with PlaylistTrait

object PlaylistComp {

  def apply(siteIdent: String, templ: PlaylistTempl): PlaylistComp =
    PlaylistComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ))

}

case class MediumComp(siteInfo: SiteComp[MediumTempl])
  extends SiteCompTrait
    with MediumTrait

object MediumComp {
  def apply(siteIdent: String, templ: MediumTempl): MediumComp =
    MediumComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ))
}
