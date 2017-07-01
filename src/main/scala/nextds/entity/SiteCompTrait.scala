package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteCompTrait
  extends SiteEntityTrait {

  def siteInfo: SiteComp[_ <: SiteTemplTrait]

  def templ: SiteTemplTrait = siteInfo.templ

  lazy val levelType: LevelType = COMP

  // all links to the level TEMPL
  def withLinkedUp(siteModel: SiteModel): Set[SiteEntityTrait] = {
    siteModel.entities(TEMPL, siteType)
      .filter(_.ident == templ.ident)
      .flatMap(_.withLinkedUp(siteModel))
      .toSet + this
  }

  // all links to the level CONF
  def withLinkedDown(siteModel: SiteModel): Set[SiteEntityTrait] = {
    siteModel.entities(CONF, siteType)
      .filter(_.asInstanceOf[SiteConfTrait].comp.ident == ident)
      .flatMap(_.withLinkedDown(siteModel))
      .toSet + this
  }


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
                                 , templ: T
                                 , title: String): SiteComp[T] = SiteComp(siteIdent, Site.nextIdent(siteIdent), templ, Some(title))

  def apply[T <: SiteTemplTrait](siteIdent: String
                                 , templ: T): SiteComp[T] = SiteComp(siteIdent, Site.nextIdent(siteIdent), templ)

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

case class MediumSize(width: Int = 1920, height: Int = 1080)

case class MediumComp(siteInfo: SiteComp[MediumTempl]
                      , url: MediumURL
                      , thumbnail: Option[MediumURL] = None
                      , size: MediumSize = MediumSize())
  extends SiteCompTrait
    with MediumTrait

object MediumComp {
  def apply(siteIdent: SiteIdent, title: String, templ: MediumTempl): MediumComp = {
    val ident = Site.nextIdent(siteIdent)
    MediumComp(SiteComp(siteIdent, ident, templ, Some(title)), image(ident), Some(thumbnail(ident)))
  }

  def image(name: String) = s"https://placeholdit.imgix.net/~text?txt=$name&w=1920&h=1080&FLEU"

  def thumbnail(name: String) = s"https://placeholdit.imgix.net/~text?txt=$name&w=40&h=40&FLEU"

}
