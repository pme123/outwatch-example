package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteConfTrait
  extends SiteEntityTrait {

  def siteConf: SiteConf[_ <: SiteCompTrait]

  lazy val siteIdent: String = siteConf.siteId
  def comp: SiteCompTrait = siteConf.comp
  lazy val levelType: LevelType = CONF
  lazy val title: String = siteConf.titleOpt.getOrElse(comp.title)
  lazy val descr: String = siteConf.descrOpt.getOrElse(comp.descr)
}

case class SiteConf[T <: SiteCompTrait](siteId: String
                                         , comp: T
                                         , titleOpt: Option[String] = None
                                         , descrOpt: Option[String] = None)


case class PlayerConf(siteConf: SiteConf[PlayerComp])
  extends SiteConfTrait
    with PlayerTrait

object PlayerConf {

  def apply(siteId: String
            , comp: PlayerComp
           , layouts: Seq[LayoutConf] = Seq()): PlayerConf =
    PlayerConf(SiteConf(siteId, comp))

}

case class LayoutConf(siteConf: SiteConf[LayoutComp]
                      , screenRegionOpt: Option[ScreenRegion] = None)
  extends SiteConfTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteConf.comp.screenRegion)
}

object LayoutConf {

  def apply(siteId: String
            , comp: LayoutComp): LayoutConf =
    LayoutConf(SiteConf(siteId, comp))

  def apply(siteId: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion): LayoutConf =
    LayoutConf(SiteConf(siteId
      , comp
      , Some(title))
      , Some(screenRegion))

}

case class RegionConf(siteConf: SiteConf[LayoutComp]
                      , screenRegionOpt: Option[ScreenRegion] = None)
  extends SiteConfTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteConf.comp.screenRegion)
}

object RegionConf {

  def apply(siteId: String
            , comp: LayoutComp): RegionConf =
    RegionConf(SiteConf(siteId, comp))

  def apply(siteId: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion): RegionConf =
    RegionConf(SiteConf(siteId
      , comp
      , Some(title))
      , Some(screenRegion))

}

case class PlaylistConf(siteConf: SiteConf[PlaylistComp])
  extends SiteConfTrait
    with PlaylistTrait

object PlaylistConf {
}

case class MediumConf(siteConf: SiteConf[MediumComp])
  extends SiteConfTrait
    with MediumTrait

object MediumConf {
}
