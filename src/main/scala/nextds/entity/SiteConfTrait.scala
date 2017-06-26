package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteConfTrait
  extends SiteEntityTrait {

  def siteConf: SiteConf[_ <: SiteCompTrait]

  def siteConfRefs: Seq[_ <: SiteConfTrait]

  lazy val siteIdent: String = siteConf.siteIdent

  lazy val ident: String = siteConf.ident

  lazy val filterTagConf: Option[FilterTagConf] = siteConf.filterTagConf


  def comp: SiteCompTrait = siteConf.comp

  lazy val levelType: LevelType = CONF
  lazy val title: String = siteConf.titleOpt.getOrElse(comp.title)
  lazy val descr: String = siteConf.descrOpt.getOrElse(comp.descr)
  lazy val maybeTitle: Option[String] = siteConf.titleOpt
  lazy val maybeDescr: Option[String] = siteConf.descrOpt

  override def filterLinks(siteEntities: Set[SiteEntityTrait]): Set[SiteEntityTrait] = {
    val partEntities = siteEntities.partition(_.levelType == CONF)
    val siteConfs = partEntities._1.map(_.asInstanceOf[SiteConfTrait])
    filterLinksLeft(siteConfs, toFilterCond) ++ filterLinksRight(siteConfs, toFilterCond) ++ partEntities._2
  }

  lazy val toFilterCond = {
    filterTagConf.map(c => FilterCond(c.condition).get).toSeq
  }

  // all links to the left - e.g. REGION > LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait]

  // all links to the right - e.g. REGION > PLAYLIST > MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait]

  protected def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond], rightSiteType: SiteType): Set[SiteEntityTrait] =
    siteConfs.filter(set => set.siteType == rightSiteType)
      .filter { lc =>
        lc.filterTagConf.forall(ftc =>
          filtersToAdhere.forall(fc => fc.adheresFilter(FilterCond(ftc.condition).get)))
      }.flatMap(lc =>
      lc.filterLinksRight(siteConfs, filtersToAdhere ++ lc.toFilterCond)) + this

  protected def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond], rightSiteType: SiteType): Set[SiteEntityTrait] =
    siteConfs.filter(set => set.siteType == rightSiteType)
      .filter { lc =>
        lc.filterTagConf.forall(ftc =>
          filtersToAdhere.forall(fc => fc.adheresFilter(FilterCond(ftc.condition).get)))
      }.flatMap(lc =>
      lc.filterLinksLeft(siteConfs, filtersToAdhere ++ lc.toFilterCond)) + this


}

case class SiteConf[T <: SiteCompTrait](siteIdent: String
                                        , ident: String
                                        , comp: T
                                        , titleOpt: Option[String] = None
                                        , descrOpt: Option[String] = None
                                        , filterTagConf: Option[FilterTagConf] = None)

object SiteConf {
  def apply[T <: SiteCompTrait](comp: T): SiteConf[T] = SiteConf(comp.siteIdent, Site.nextIdent(comp.siteIdent), comp)

  def apply[T <: SiteCompTrait](comp: T, filterTagConf: FilterTagConf): SiteConf[T] =
    SiteConf(comp.siteIdent, Site.nextIdent(comp.siteIdent), comp
      , filterTagConf = Some(filterTagConf))

  def apply[T <: SiteCompTrait](comp: T, title: String): SiteConf[T] = SiteConf(comp.siteIdent, Site.nextIdent(comp.siteIdent), comp, Some(title))

}


case class PlayerConf(siteConf: SiteConf[PlayerComp]
                      , siteConfRefs: Seq[LayoutConf])
  extends SiteConfTrait
    with PlayerTrait {

  override def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait =
    copy(siteConfRefs = siteConfRefs :+ siteEntity.asInstanceOf[LayoutConf])

  // all links to the left - nothing
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    Set(this)

  // all links to the right - LAYOUT > REGION > PLAYLIST > MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksRight(siteConfs,filtersToAdhere, LAYOUT)
}

object PlayerConf {

  def apply(comp: PlayerComp): PlayerConf =
    PlayerConf(SiteConf(comp), Nil)

  def apply(comp: PlayerComp
            , filterTagConf: FilterTagConf): PlayerConf =
    PlayerConf(SiteConf(comp
      , filterTagConf = filterTagConf), Nil)

  def apply(siteIdent: String
            , comp: PlayerComp
            , layoutConfs: Seq[LayoutConf] = Nil): PlayerConf =
    PlayerConf(SiteConf(siteIdent, Site.nextIdent(siteIdent), comp), layoutConfs)

}

case class LayoutConf(siteConf: SiteConf[LayoutComp]
                      , screenRegionOpt: Option[ScreenRegion] = None
                      , siteConfRefs: Seq[RegionConf] = Nil)
  extends SiteConfTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteConf.comp.screenRegion)
  lazy val maybeScreenRegion: Option[ScreenRegion] = screenRegionOpt

  override def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait =
    copy(siteConfRefs = siteConfRefs :+ siteEntity.asInstanceOf[RegionConf])

  // all links to the left - PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs,filtersToAdhere, PLAYER)

  // all links to the right - REGION > PLAYLIST > MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksRight(siteConfs,filtersToAdhere, REGION)

}

object LayoutConf {

  def apply(siteIdent: String
            , comp: LayoutComp): LayoutConf =
    LayoutConf(SiteConf(siteIdent, Site.nextIdent(siteIdent), comp))

  def apply(comp: LayoutComp
            , filterTagConf: FilterTagConf): LayoutConf =
    LayoutConf(SiteConf(comp
      , filterTagConf = filterTagConf))

  def apply(siteIdent: String
            , comp: LayoutComp
            , regionConfs: Seq[RegionConf]): LayoutConf =
    LayoutConf(SiteConf(siteIdent, Site.nextIdent(siteIdent), comp), siteConfRefs = regionConfs)

  def apply(siteIdent: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion
            , regionConfs: Seq[RegionConf]): LayoutConf =
    LayoutConf(SiteConf(siteIdent
      , Site.nextIdent(siteIdent)
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

  override def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait =
    copy(siteConfRefs = siteConfRefs :+ siteEntity.asInstanceOf[PlaylistConf])

  // all links to the left - LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs,filtersToAdhere, LAYOUT)

  // all links to the right - PLAYLIST > MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksRight(siteConfs,filtersToAdhere, PLAYLIST)
}

object RegionConf {

  def apply(siteIdent: String
            , comp: LayoutComp): RegionConf =
    RegionConf(SiteConf(siteIdent, Site.nextIdent(siteIdent), comp))

  def apply(comp: LayoutComp
            , filterTagConf: FilterTagConf): RegionConf =
    RegionConf(SiteConf(comp
      , filterTagConf = filterTagConf))

  def apply(siteIdent: String
            , comp: LayoutComp
            , playlistConfs: Seq[PlaylistConf]): RegionConf =
    RegionConf(SiteConf(siteIdent, Site.nextIdent(siteIdent), comp), siteConfRefs = playlistConfs)

  def apply(siteIdent: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion
            , playlistConfs: Seq[PlaylistConf]): RegionConf =
    RegionConf(SiteConf(siteIdent
      , Site.nextIdent(siteIdent)
      , comp
      , Some(title))
      , Some(screenRegion)
      , playlistConfs)

}

case class PlaylistConf(siteConf: SiteConf[PlaylistComp]
                        , siteConfRefs: Seq[MediumConf])
  extends SiteConfTrait
    with PlaylistTrait {
  override def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait =
    copy(siteConfRefs = siteConfRefs :+ siteEntity.asInstanceOf[MediumConf])

  // all links to the left - REGION > LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs,filtersToAdhere, REGION)

  // all links to the right - MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksRight(siteConfs,filtersToAdhere, MEDIUM)

}

object PlaylistConf {
  def apply(siteIdent: String
            , comp: PlaylistComp): PlaylistConf =
    PlaylistConf(SiteConf(siteIdent, Site.nextIdent(siteIdent), comp), Nil)

  def apply(comp: PlaylistComp
            , filterTagConf: FilterTagConf): PlaylistConf =
    PlaylistConf(SiteConf(comp
      , filterTagConf = filterTagConf), Nil)

}

case class MediumConf(siteConf: SiteConf[MediumComp])
  extends SiteConfTrait
    with MediumTrait {
  // a MediumConf has no refs to other Confs
  val siteConfRefs = Nil

  // all links to the left - PLAYLIST > REGION > LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs,filtersToAdhere, PLAYLIST)

  // all links to the right - nothing
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    Set(this)
}

object MediumConf {
  def apply(siteIdent: String
            , comp: MediumComp): MediumConf =
    MediumConf(SiteConf(siteIdent, Site.nextIdent(siteIdent), comp))

  def apply(comp: MediumComp
            , filterTagConf: FilterTagConf): MediumConf =
    MediumConf(SiteConf(comp
      , filterTagConf = filterTagConf))

}
