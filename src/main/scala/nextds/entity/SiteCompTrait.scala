package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteCompTrait
  extends SiteEntityTrait {

  def siteComp: SiteComp[_ <: SiteTemplTrait]

  lazy val siteIdent: String = siteComp.siteIdent
  lazy val ident: String = siteComp.ident

  def templ: SiteTemplTrait = siteComp.templ

  lazy val levelType: LevelType = COMP
  lazy val title: String = siteComp.titleOpt.getOrElse(templ.title)
  lazy val descr: String = siteComp.descrOpt.getOrElse(templ.descr)
  lazy val maybeTitle: Option[String] = siteComp.titleOpt
  lazy val maybeDescr: Option[String] = siteComp.descrOpt

}

case class SiteComp[T <: SiteTemplTrait](siteIdent: String
                                         , ident: String
                                         , templ: T
                                         , titleOpt: Option[String] = None
                                         , descrOpt: Option[String] = None)

object SiteComp {
  def apply[T <: SiteTemplTrait](templ: T): SiteComp[T] = SiteComp(templ.siteIdent, Site.nextIdent(templ.siteIdent), templ)
  def apply[T <: SiteTemplTrait](siteIdent: String
                                 ,templ: T
                                 , title:String): SiteComp[T] = SiteComp(siteIdent, Site.nextIdent(siteIdent), templ, Some(title))
  def apply[T <: SiteTemplTrait](siteIdent: String
                                 ,templ: T): SiteComp[T] = SiteComp(siteIdent, Site.nextIdent(siteIdent), templ)

}

case class PlayerComp(siteComp: SiteComp[PlayerTempl]
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

case class LayoutComp(siteComp: SiteComp[LayoutTempl]
                      , screenRegionOpt: Option[ScreenRegion] = None)
  extends SiteCompTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteComp.templ.screenRegion)
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

case class PlaylistComp(siteComp: SiteComp[PlaylistTempl])
  extends SiteCompTrait
    with PlaylistTrait

object PlaylistComp {

  def apply(siteIdent: String, templ: PlaylistTempl): PlaylistComp =
    PlaylistComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ))

}

case class MediumComp(siteComp: SiteComp[MediumTempl])
  extends SiteCompTrait
    with MediumTrait

object MediumComp {
  def apply(siteIdent: String, templ: MediumTempl): MediumComp =
    MediumComp(SiteComp(siteIdent, Site.nextIdent(siteIdent), templ))
}
