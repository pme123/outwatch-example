package nextds.server.control

import nextds.entity.ScreenRegion._
import nextds.entity.{REGION, _}

import scala.util.Random

object SitesCreator {
  val allSites: Seq[SiteIdent] = Seq("PUBLIC", "MGAA", "COM2A", "BETXY")

  def siteIdent: SiteIdent = oneOfSeq(allSites)

  def nextSiteIdent: SiteIdent = Site.nextIdent(siteIdent)

  def oneOf[T](xs: T*): T = xs(Random.nextInt(xs.length))

  def oneOfSeq[T](xs: Seq[T]): T = oneOf(xs: _*)

  def entitiesFor[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): Seq[T] =
    (levelType match {
      case TEMPL => SiteTemplCreator.allTempls(siteType)
      case COMP => SiteCompCreator.allComps(siteType)
      case CONF => SiteConfCreator.allConfs(siteType)
      case FILTER => SiteFilterCreator.allConfs(siteType)
      case other => //TODO
        throw new IllegalArgumentException(s"Unsupported Level Type: $other")
    }).map(_.asInstanceOf[T])

}

object SiteTemplCreator {

  import SitesCreator._

  val entityCount = 20

  private def siteTempl(label: String) = for {i <- 0 to entityCount} yield {
    val ident = siteIdent
    SiteTempl(ident, Site.nextIdent(ident)
      , oneOf(s"$label 2.3", s"$i.0 $label", s"${label.take(2)}-AZ-$i")
      , oneOf(s"Special Configs for this $label $i", s"No description for $label $i available"))
  }

  val allTempls: Map[SiteType, Seq[SiteTemplTrait]] = Map(
    PLAYER -> siteTempl("Player").map(PlayerTempl.apply)
    , LAYOUT -> siteTempl("Layout").map(LayoutTempl(_, oneOf(ultraHD4K, fullHD)))
    , REGION -> Nil
    , PLAYLIST -> siteTempl("Playlist").map(PlaylistTempl.apply)
    , MEDIUM -> siteTempl("Medium").map(MediumTempl.apply)
  )

  def templ(siteType: SiteType): SiteTemplTrait = oneOfSeq(allTempls(siteType))
}

object SiteCompCreator {
  import SiteTemplCreator._
  import SitesCreator._

  val entityCount = 20

  private def siteComp[T <: SiteTemplTrait](templ: T): Seq[SiteComp[T]] = for {i <- 0 to entityCount} yield {
    val ident = siteIdent
    val siteType = templ.siteType
    val label =  siteType.label
    SiteComp[T](ident, Site.nextIdent(ident)
      , oneOf(templ)
      , oneOf(None, Some(s"$label 2.3"), Some(s"$i.0 $label"), Some(s"${label.take(2)}-COMP-$i"))
     , oneOf(None, None, Some(s"Special Comp for this $label $i"), Some(s"No description for $label $i available"))
    )
  }

 private def lat =  randomCoord(46.437, 47.522)
 private def lng =  randomCoord(6.67145, 9.34662)
 private def randomCoord(min:Double, max: Double) =
    min + (Random.nextDouble() * (max - min))
  private def location = oneOf(None, Some(PlayerLocation(lat, lng)), Some(PlayerLocation(lat, lng)))
  private def playerStatus: PlayerStatus = oneOf(PlayerStatus.STOPPED,PlayerStatus.NOT_CONNECTED, PlayerStatus.RUNNING)

  val allComps: Map[SiteType, Seq[SiteCompTrait]] = Map(
    PLAYER -> siteComp(templ(PLAYER))
      .map(_.asInstanceOf[SiteComp[PlayerTempl]])
      .map(PlayerComp(_, playerStatus, location))
    , LAYOUT -> siteComp(templ(LAYOUT))
      .map(_.asInstanceOf[SiteComp[LayoutTempl]])
      .map(LayoutComp(_, oneOf(None, Some(ScreenRegion(fromTop = 40, width = 333, height=222)))))
    , REGION -> Nil
    , PLAYLIST -> siteComp(templ(PLAYLIST))
      .map(_.asInstanceOf[SiteComp[PlaylistTempl]])
      .map(PlaylistComp(_))
    , MEDIUM -> siteComp(templ(MEDIUM))
      .map(_.asInstanceOf[SiteComp[MediumTempl]])
      .map(MediumComp(_))
  )

  def comp(siteType: SiteType): SiteCompTrait = oneOfSeq(allComps(siteType))

}

object SiteConfCreator {

  import SiteCompCreator._
  import SitesCreator._

  val entityCount = 20

  private def siteConf(comp: SiteCompTrait): Seq[SiteConf[SiteCompTrait]] = for {i <- 0 to entityCount} yield {
    val ident = siteIdent
    val siteType = comp.siteType
    val label =  siteType.label
    SiteConf(ident, Site.nextIdent(ident)
      , comp
      , oneOf(None,None, Some(s"$label 2.3"), Some(s"$i.0 $label"), Some(s"${label.take(2)}-CONF-$i"))
      , oneOf(None,Some(s"Special Config for this $label $i"))
    )
  }

  private val mediumConfs = siteConf(comp(MEDIUM))
    .map(_.asInstanceOf[SiteConf[MediumComp]])
    .map(MediumConf(_))
  private val playlistConfs = siteConf(comp(PLAYLIST))
    .map(_.asInstanceOf[SiteConf[PlaylistComp]])
    .map(PlaylistConf(_, Set(oneOfSeq(mediumConfs),oneOfSeq(mediumConfs),oneOfSeq(mediumConfs)).toSeq))
  private def screenRegion = oneOf(None, None, Some(ScreenRegion(12, 24)))
  private val regionConfs = siteConf(comp(LAYOUT))
    .map(_.asInstanceOf[SiteConf[LayoutComp]])
    .map(RegionConf(_, screenRegion, Seq(oneOfSeq(playlistConfs))))
  private val layoutConfs = siteConf(comp(LAYOUT))
    .map(_.asInstanceOf[SiteConf[LayoutComp]])
    .map(LayoutConf(_, screenRegion,  Seq(oneOfSeq(regionConfs))))
  private val playerConfs = siteConf(comp(PLAYER))
    .map(_.asInstanceOf[SiteConf[PlayerComp]])
    .map(c=>PlayerConf(c,Seq(oneOfSeq(layoutConfs))))


  val allConfs: Map[SiteType, Seq[SiteConfTrait]] = Map(
    PLAYER -> playerConfs
    , LAYOUT -> layoutConfs
    , REGION -> regionConfs
    , PLAYLIST -> playlistConfs
    , MEDIUM -> mediumConfs
  )

}

object SiteFilterCreator {

  val publicSiteIdent: SiteIdent = SitesRepo.allSites().head
  val defaultSiteIdent: SiteIdent = SitesRepo.allSites()(1)

  private val tagFilters = Seq(
    TagFilter(SiteFilter(publicSiteIdent, "Languages"))
    , TagFilter(SiteFilter(defaultSiteIdent, "Stores"))
    , TagFilter(SiteFilter(defaultSiteIdent, "Scales"))
  )
  private val timeFilters = Seq(
    TimeFilter(SiteFilter(publicSiteIdent, "Weekdays"))
    , TimeFilter(SiteFilter(publicSiteIdent, "Weekends"))
    , TimeFilter(SiteFilter(defaultSiteIdent, "Opening Hours"))
  )

  val allConfs: Map[SiteType, Seq[SiteFilterTrait]] = Map(
    TAG_FILTER -> tagFilters
    , TIME_FILTER -> timeFilters
  )
}

