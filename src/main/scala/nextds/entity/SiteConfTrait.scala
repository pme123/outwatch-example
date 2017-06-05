package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteConfTrait
  extends SiteEntityTrait {

  def siteConf: SiteConf[_ <: SiteCompTrait]

  def siteConfRefs: Seq[_ <: SiteConfTrait]

  lazy val siteIdent: String = siteConf.siteId

  def comp: SiteCompTrait = siteConf.comp

  lazy val levelType: LevelType = CONF
  lazy val title: String = siteConf.titleOpt.getOrElse(comp.title)
  lazy val descr: String = siteConf.descrOpt.getOrElse(comp.descr)
  lazy val maybeTitle: Option[String] = siteConf.titleOpt
  lazy val maybeDescr: Option[String] = siteConf.descrOpt

}

case class SiteConf[T <: SiteCompTrait](siteId: String
                                        , comp: T
                                        , titleOpt: Option[String] = None
                                        , descrOpt: Option[String] = None)

object SiteConf {
  def apply[T <: SiteCompTrait](comp: T): SiteConf[T] = SiteConf(comp.siteIdent, comp)

}


case class PlayerConf(siteConf: SiteConf[PlayerComp]
                      , siteConfRefs: Seq[LayoutConf])
  extends SiteConfTrait
    with PlayerTrait {

}

object PlayerConf {

  def apply(comp: PlayerComp):PlayerConf =
    PlayerConf(SiteConf(comp), Nil)

  def apply(siteId: String
            , comp: PlayerComp
            , layoutConfs: Seq[LayoutConf] = Nil): PlayerConf =
    PlayerConf(SiteConf(siteId, comp), layoutConfs)

}

case class LayoutConf(siteConf: SiteConf[LayoutComp]
                      , screenRegionOpt: Option[ScreenRegion] = None
                      , siteConfRefs: Seq[RegionConf] = Nil)
  extends SiteConfTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteConf.comp.screenRegion)
  lazy val maybeScreenRegion: Option[ScreenRegion] = screenRegionOpt

}

object LayoutConf {

  def apply(siteIdent: String
            , comp: LayoutComp):LayoutConf =
    LayoutConf(SiteConf(siteIdent, comp))

  def apply(siteIdent: String
            , comp: LayoutComp
            , regionConfs: Seq[RegionConf]): LayoutConf =
    LayoutConf(SiteConf(siteIdent, comp), siteConfRefs = regionConfs)

  def apply(siteIdent: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion
            , regionConfs: Seq[RegionConf]): LayoutConf =
    LayoutConf(SiteConf(siteIdent
      , comp
      , Some(title))
      , Some(screenRegion)
      , regionConfs)

}

case class RegionConf(siteConf: SiteConf[LayoutComp]
                      , screenRegionOpt: Option[ScreenRegion] = None
                      , siteConfRefs: Seq[PlaylistConf] = Nil)
  extends SiteConfTrait
    with RegionTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteConf.comp.screenRegion)
}

object RegionConf {

  def apply(siteIdent: String
            , comp: LayoutComp):RegionConf =
    RegionConf(SiteConf(siteIdent, comp))

  def apply(siteIdent: String
            , comp: LayoutComp
            , playlistConfs: Seq[PlaylistConf]): RegionConf =
    RegionConf(SiteConf(siteIdent, comp), siteConfRefs = playlistConfs)

  def apply(siteIdent: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion
            , playlistConfs: Seq[PlaylistConf]): RegionConf =
    RegionConf(SiteConf(siteIdent
      , comp
      , Some(title))
      , Some(screenRegion)
      , playlistConfs)

}

case class PlaylistConf(siteConf: SiteConf[PlaylistComp]
                       , siteConfRefs: Seq[MediumConf] = Nil)
  extends SiteConfTrait
    with PlaylistTrait

object PlaylistConf {
  def apply(siteIdent: String
            , comp: PlaylistComp):PlaylistConf =
    PlaylistConf(SiteConf(siteIdent, comp))

}

case class MediumConf(siteConf: SiteConf[MediumComp])
  extends SiteConfTrait
    with MediumTrait{
  // a MediumConf has no refs to other Confs
  val siteConfRefs = Nil
}

object MediumConf {
  def apply(siteIdent: String
            , comp: MediumComp): MediumConf =
    MediumConf(SiteConf(siteIdent, comp))
}
