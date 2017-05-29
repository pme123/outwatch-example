package nextds.entity

import outwatch.dom.VNode

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteCompTrait
  extends SiteEntityTrait {

  def siteComp: SiteComp[_ <: SiteTemplTrait]

  lazy val siteIdent: String = siteComp.siteId
  def templ: SiteTemplTrait = siteComp.templ
  lazy val levelType: LevelType = COMP
  lazy val title: String = siteComp.titleOpt.getOrElse(templ.title)
  lazy val descr: String = siteComp.descrOpt.getOrElse(templ.descr)
  lazy val maybeTitle: Option[String] = siteComp.titleOpt
  lazy val maybeDescr: Option[String] = siteComp.descrOpt

}

case class SiteComp[T <: SiteTemplTrait](siteId: String
                                         , templ: T
                                         , titleOpt: Option[String] = None
                                         , descrOpt: Option[String] = None)


case class PlayerComp(siteComp: SiteComp[PlayerTempl]
                      , status: PlayerStatus = PlayerStatus.NOT_CONNECTED
                      , maybeLocation: Option[PlayerLocation] = None)
  extends SiteCompTrait
    with PlayerTrait

object PlayerComp {

  def apply(siteId: String
            , templ: PlayerTempl
            , title: String): PlayerComp =
    PlayerComp(SiteComp(siteId, templ, Some(title)))

  def apply(siteId: String
            , templ: PlayerTempl
            , title: String
            , status: PlayerStatus
            , location: PlayerLocation): PlayerComp =
    PlayerComp(SiteComp(siteId, templ, Some(title))
      , status, Some(location))
}

sealed trait PlayerStatus

object PlayerStatus {

  case object RUNNING extends PlayerStatus

  case object STOPPED extends PlayerStatus

  case object NOT_CONNECTED extends PlayerStatus

}

case class PlayerLocation(lat: Double, lng: Double)

case class LayoutComp(siteComp: SiteComp[LayoutTempl]
                      , screenRegionOpt: Option[ScreenRegion] = None)
  extends SiteCompTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteComp.templ.screenRegion)
  lazy val maybeScreenRegion: Option[ScreenRegion] = screenRegionOpt

}

object LayoutComp {

  def apply(siteId: String
            , templ: LayoutTempl): LayoutComp =
    LayoutComp(SiteComp(siteId, templ))

  def apply(siteId: String
            , templ: LayoutTempl
            , title: String
            , screenRegion: ScreenRegion): LayoutComp =
    LayoutComp(SiteComp(siteId
      , templ
      , Some(title))
      , Some(screenRegion))

}

case class PlaylistComp(siteComp: SiteComp[PlaylistTempl])
  extends SiteCompTrait
    with PlaylistTrait

object PlaylistComp {
}

case class MediumComp(siteComp: SiteComp[MediumTempl])
  extends SiteCompTrait
    with MediumTrait

object MediumComp {
}
