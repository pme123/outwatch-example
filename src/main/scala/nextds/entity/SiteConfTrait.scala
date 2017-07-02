package nextds.entity

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteConfTrait
  extends SiteEntityTrait {

  def siteInfo: SiteConfInfo[_ <: SiteCompTrait]

  def siteConfRefs: Seq[_ <: SiteConfTrait]

  lazy val filterTagConf: Option[FilterTagConf] = siteInfo.filterTagConf
  lazy val timingConf: Option[TimingConf] = siteInfo.timingConf

  def comp: SiteCompTrait = siteInfo.comp

  lazy val levelType: LevelType = CONF

  override def filterLinks(siteEntities: Set[SiteEntityTrait]): Set[SiteEntityTrait] = {
    val partEntities = siteEntities.partition(_.levelType == CONF)
    val siteConfs = partEntities._1.map(_.asInstanceOf[SiteConfTrait])
    filterLinksLeft(siteConfs, toFilterCond) ++ filterLinksRight(siteConfs, toFilterCond) ++ partEntities._2
  }

  lazy val toFilterCond: Seq[FilterCond] = {
    filterTagConf.map(ftc => ftc.filterCond).toSeq
  }

  override def withLinks(siteModel: SiteModel): Set[SiteEntityTrait] =
    withLinkedDown(siteModel) ++ withLinkedUp(siteModel) ++
      withLinkedConfRight(siteModel.level(CONF)) ++
      withLinkedConfLeft(siteType, siteModel.level(CONF), Seq(ident))


  // all links to the level COMP
  def withLinkedUp(siteModel: SiteModel): Set[SiteEntityTrait] = {
    val siteT = if (siteType == REGION) LAYOUT else siteType
    println("linkedUp")
    siteModel.entities(COMP, siteT)
      .filter(_.ident == comp.ident)
      .flatMap(_.withLinkedUp(siteModel))
      .toSet + this
  }

  // all links to the level Filter
  def withLinkedDown(siteModel: SiteModel): Set[SiteEntityTrait] = {
    println("linkedDown")
    filterTagConf
      .toSeq
      .flatMap(f => f.withLinkedDown(siteModel))
      .toSet ++
      timingConf
        .toSeq
        .flatMap(t => t.withLinkedDown(siteModel)) + this
  }


  // all links to the right - e.g. REGION > PLAYLIST > MEDIUM
  def withLinkedConfRight(siteLevel: SiteLevel): Set[SiteEntityTrait] = {
    siteConfRefs.flatMap(e => e.withLinkedConfRight(siteLevel)).toSet + this
  }

  // all links to the left - e.g. REGION > LAYOUT > PLAYER
  // implemented with Breadth-first search
  def withLinkedConfLeft(siteType: SiteType, siteLevel: SiteLevel, refIdents: Seq[SiteEntityIdent]): Set[SiteEntityTrait] = {

    def inner(leftType: SiteType): Set[SiteEntityTrait] = {
      val toDoes = (for {
        set <- siteLevel.entities(leftType).entities.map(_.asInstanceOf[SiteConfTrait])
        er <- set.siteConfRefs
        if refIdents.contains(er.ident)
      } yield set).distinct.toList
      withLinkedConfLeft(leftType, siteLevel, toDoes.map(_.ident)) ++ toDoes
    }

    siteType match {
      case LAYOUT => inner(PLAYER)
      case REGION => inner(LAYOUT)
      case PLAYLIST => inner(REGION)
      case MEDIUM => inner(PLAYLIST)
      case _ => Set()
    }
  }


  // all links to the left - e.g. REGION > LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait]

  // all links to the right - e.g. REGION > PLAYLIST > MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait]

  protected def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond], rightSiteType: SiteType): Set[SiteEntityTrait] =
    siteConfs.filter(set => set.siteType == rightSiteType)
      .filter { lc =>
        lc.filterTagConf.forall(ftc =>
          filtersToAdhere.forall(fc => fc.adheresFilter(ftc.filterCond)))
      }.flatMap(lc =>
      lc.filterLinksRight(siteConfs, filtersToAdhere ++ lc.toFilterCond)) + this

  protected def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond], rightSiteType: SiteType): Set[SiteEntityTrait] =
    siteConfs.filter(set => set.siteType == rightSiteType)
      .filter { lc =>
        lc.filterTagConf.forall(ftc =>
          filtersToAdhere.forall(fc => fc.adheresFilter(ftc.filterCond)))
      }.flatMap(lc =>
      lc.filterLinksLeft(siteConfs, filtersToAdhere ++ lc.toFilterCond)) + this


}

case class SiteConfInfo[T <: SiteCompTrait](siteIdent: String
                                            , ident: String
                                            , comp: T
                                            , maybeTitle: Option[String] = None
                                            , maybeDescr: Option[String] = None
                                            , filterTagConf: Option[FilterTagConf] = None
                                            , timingConf: Option[TimingConf] = None)
  extends SiteEntityInfoTrait {
  lazy val title: String = maybeTitle.getOrElse(comp.title)
  lazy val descr: String = maybeDescr.getOrElse(comp.descr)

}

object SiteConfInfo {
  def apply[T <: SiteCompTrait](comp: T): SiteConfInfo[T] = SiteConfInfo(comp.siteIdent, Site.nextIdent(comp.siteIdent), comp)

  def apply[T <: SiteCompTrait](comp: T, filterTagConf: FilterTagConf): SiteConfInfo[T] =
    SiteConfInfo(comp.siteIdent, Site.nextIdent(comp.siteIdent), comp
      , filterTagConf = Some(filterTagConf))

  def apply[T <: SiteCompTrait](comp: T, title: String): SiteConfInfo[T] =
    SiteConfInfo(comp.siteIdent, Site.nextIdent(comp.siteIdent), comp, Some(title))

  def apply[T <: SiteCompTrait](comp: T, title: String, filterTagConf: FilterTagConf): SiteConfInfo[T] =
    SiteConfInfo(comp.siteIdent
      , Site.nextIdent(comp.siteIdent)
      , comp
      , Some(title)
      , filterTagConf = Some(filterTagConf))

  def apply[T <: SiteCompTrait](comp: T, title: String, timingConf: TimingConf): SiteConfInfo[T] =
    SiteConfInfo(comp.siteIdent
      , Site.nextIdent(comp.siteIdent)
      , comp
      , Some(title)
      , timingConf = Some(timingConf))

}


case class PlayerConf(siteInfo: SiteConfInfo[PlayerComp]
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
    filterLinksRight(siteConfs, filtersToAdhere, LAYOUT)
}

object PlayerConf {

  def apply(comp: PlayerComp): PlayerConf =
    PlayerConf(SiteConfInfo(comp), Nil)

  def apply(comp: PlayerComp
            , filterTagConf: FilterTagConf): PlayerConf =
    PlayerConf(SiteConfInfo(comp
      , filterTagConf = filterTagConf), Nil)

  def apply(siteIdent: String
            , comp: PlayerComp
            , layoutConfs: Seq[LayoutConf] = Nil): PlayerConf =
    PlayerConf(SiteConfInfo(siteIdent, Site.nextIdent(siteIdent), comp), layoutConfs)

}

case class LayoutConf(siteInfo: SiteConfInfo[LayoutComp]
                      , screenRegionOpt: Option[ScreenRegion] = None
                      , siteConfRefs: Seq[RegionConf] = Nil)
  extends SiteConfTrait
    with LayoutTrait {
  lazy val screenRegion: ScreenRegion = screenRegionOpt.getOrElse(siteInfo.comp.screenRegion)
  lazy val maybeScreenRegion: Option[ScreenRegion] = screenRegionOpt

  override def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait =
    copy(siteConfRefs = siteConfRefs :+ siteEntity.asInstanceOf[RegionConf])

  // all links to the left - PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs, filtersToAdhere, PLAYER)

  // all links to the right - REGION > PLAYLIST > MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksRight(siteConfs, filtersToAdhere, REGION)

}

object LayoutConf {

  def apply(siteIdent: String
            , comp: LayoutComp): LayoutConf =
    LayoutConf(SiteConfInfo(siteIdent, Site.nextIdent(siteIdent), comp))

  def apply(comp: LayoutComp
            , filterTagConf: FilterTagConf): LayoutConf =
    LayoutConf(SiteConfInfo(comp
      , filterTagConf = filterTagConf))

  def apply(siteIdent: String
            , comp: LayoutComp
            , regionConfs: Seq[RegionConf]): LayoutConf =
    LayoutConf(SiteConfInfo(siteIdent, Site.nextIdent(siteIdent), comp), siteConfRefs = regionConfs)

  def apply(siteIdent: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion
            , regionConfs: Seq[RegionConf]): LayoutConf =
    LayoutConf(SiteConfInfo(siteIdent
      , Site.nextIdent(siteIdent)
      , comp
      , Some(title))
      , Some(screenRegion)
      , regionConfs)

}

case class RegionConf(siteInfo: SiteConfInfo[LayoutComp]
                      , maybeScreenRegion: Option[ScreenRegion] = None
                      , siteConfRefs: Seq[PlaylistConf] = Nil)
  extends SiteConfTrait
    with RegionTrait {

  lazy val screenRegion: ScreenRegion = maybeScreenRegion.getOrElse(siteInfo.comp.screenRegion)

  override def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait =
    copy(siteConfRefs = siteConfRefs :+ siteEntity.asInstanceOf[PlaylistConf])

  // all links to the left - LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs, filtersToAdhere, LAYOUT)

  // all links to the right - PLAYLIST > MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksRight(siteConfs, filtersToAdhere, PLAYLIST)
}

object RegionConf {

  def apply(siteIdent: String
            , comp: LayoutComp): RegionConf =
    RegionConf(SiteConfInfo(siteIdent, Site.nextIdent(siteIdent), comp))

  def apply(comp: LayoutComp
            , filterTagConf: FilterTagConf): RegionConf =
    RegionConf(SiteConfInfo(comp
      , filterTagConf = filterTagConf))

  def apply(siteIdent: String
            , comp: LayoutComp
            , playlistConfs: Seq[PlaylistConf]): RegionConf =
    RegionConf(SiteConfInfo(siteIdent, Site.nextIdent(siteIdent), comp), siteConfRefs = playlistConfs)

  def apply(siteIdent: String
            , comp: LayoutComp
            , title: String
            , screenRegion: ScreenRegion
            , playlistConfs: Seq[PlaylistConf]): RegionConf =
    RegionConf(SiteConfInfo(siteIdent
      , Site.nextIdent(siteIdent)
      , comp
      , Some(title))
      , Some(screenRegion)
      , playlistConfs)

}

case class PlaylistConf(siteInfo: SiteConfInfo[PlaylistComp]
                        , siteConfRefs: Seq[MediumConf])
  extends SiteConfTrait
    with PlaylistTrait {
  override def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait =
    copy(siteConfRefs = siteConfRefs :+ siteEntity.asInstanceOf[MediumConf])

  // all links to the left - REGION > LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs, filtersToAdhere, REGION)

  // all links to the right - MEDIUM
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksRight(siteConfs, filtersToAdhere, MEDIUM)

}

object PlaylistConf {
  def apply(siteIdent: String
            , comp: PlaylistComp): PlaylistConf =
    PlaylistConf(SiteConfInfo(siteIdent, Site.nextIdent(siteIdent), comp), Nil)

  def apply(comp: PlaylistComp
            , filterTagConf: FilterTagConf): PlaylistConf =
    PlaylistConf(SiteConfInfo(comp
      , filterTagConf = filterTagConf), Nil)

}

case class MediumConf(siteInfo: SiteConfInfo[MediumComp], durationInMs: Long = 10 * 1000)
  extends SiteConfTrait
    with MediumTrait {

  lazy val durationInSec: Double = durationInMs / 1000
  lazy val durationInSecStr: String = f"$durationInSec%1.2f"
  // a MediumConf has no refs to other Confs
  val siteConfRefs = Nil
  lazy val siteComp: MediumComp = siteInfo.comp

  // all links to the left - PLAYLIST > REGION > LAYOUT > PLAYER
  def filterLinksLeft(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    filterLinksLeft(siteConfs, filtersToAdhere, PLAYLIST)

  // all links to the right - nothing
  def filterLinksRight(siteConfs: Set[SiteConfTrait], filtersToAdhere: Seq[FilterCond]): Set[SiteEntityTrait] =
    Set(this)
}

object MediumConf {
  def apply(siteIdent: String
            , comp: MediumComp): MediumConf =
    MediumConf(SiteConfInfo(siteIdent, Site.nextIdent(siteIdent), comp))

  def apply(comp: MediumComp
            , filterTagConf: FilterTagConf): MediumConf =
    MediumConf(SiteConfInfo(comp
      , filterTagConf = filterTagConf))

}
