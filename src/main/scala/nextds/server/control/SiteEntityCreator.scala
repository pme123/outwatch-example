package nextds.server.control

import nextds.entity.ScreenRegion._
import nextds.entity.{REGION, _}

import scala.util.Random

object SitesCreator {
  val allSites: Seq[SiteIdent] = Seq("PUBL2", "SFN", "COM2A", "BETXY")

  def siteIdent: SiteIdent = oneOfSeq(allSites)

  def nextSiteIdent: SiteIdent = Site.nextIdent(siteIdent)

  def oneOf[T](xs: T*): T = xs(Random.nextInt(xs.length))

  def oneOfSeq[T](xs: Seq[T]): T = oneOf(xs: _*)

  lazy val siteModel: SiteModel = SiteModel(
    SiteTemplCreator.siteLevel
    , SiteTemplCreator.siteLevel
    , SiteTemplCreator.siteLevel
  )

  def entitiesFor[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): SiteEntities[_ <: SiteEntityTrait] =
    levelType match {
      case TEMPL => SiteTemplCreator.allTempls(siteType)
      case COMP => SiteCompCreator.allComps(siteType)
      case CONF => SiteConfCreator.allConfs(siteType)
      case other => //TODO
        throw new IllegalArgumentException(s"Unsupported Level Type: $other")
    }

}

object SiteTemplCreator {

  import SitesCreator._

  val entityCount = 20

  private def siteTempl(label: String) = for {i <- 0 to entityCount} yield {
    val ident = siteIdent
    SiteEntityInfo(ident, Site.nextIdent(ident)
      , oneOf(s"$label 2.3", s"$i.0 $label", s"${label.take(2)}-AZ-$i")
      , oneOf(s"Special Configs for this $label $i", s"No description for $label $i available"))
  }

  val allTempls: Map[SiteType, SiteEntities[SiteTemplTrait]] = Map(
    PLAYER -> SiteEntities(TEMPL, PLAYER, siteTempl("Player").map(PlayerTempl.apply)
    ), LAYOUT -> SiteEntities(TEMPL, LAYOUT, siteTempl("Layout").map(LayoutTempl(_, oneOf(ultraHD4K, fullHD)))
    ), REGION -> SiteEntities(TEMPL, REGION, Seq(
    )), PLAYLIST -> SiteEntities(TEMPL, PLAYLIST, siteTempl("Playlist").map(PlaylistTempl.apply)
    ), MEDIUM -> SiteEntities(TEMPL, MEDIUM, siteTempl("Medium").map(MediumTempl.apply)
    ))

  def templ(siteType: SiteType): SiteTemplTrait = oneOfSeq(allTempls(siteType).entities)

  lazy val siteLevel = SiteLevel(TEMPL, allTempls)

}

object SiteCompCreator {

  import SiteTemplCreator._
  import SitesCreator._

  val entityCount = 20

  private def siteComp[T <: SiteTemplTrait](templ: T): Seq[SiteComp[T]] = for {i <- 0 to entityCount} yield {
    val ident = siteIdent
    val siteType = templ.siteType
    val label = siteType.label
    SiteComp[T](ident, Site.nextIdent(ident)
      , oneOf(templ)
      , oneOf(None, Some(s"$label 2.3"), Some(s"$i.0 $label"), Some(s"${label.take(2)}-COMP-$i"))
      , oneOf(None, None, Some(s"Special Comp for this $label $i"), Some(s"No description for $label $i available"))
    )
  }

  private def lat = randomCoord(46.437, 47.522)

  private def lng = randomCoord(6.67145, 9.34662)

  private def randomCoord(min: Double, max: Double) =
    min + (Random.nextDouble() * (max - min))

  private def location = oneOf(None, Some(PlayerLocation(lat, lng)), Some(PlayerLocation(lat, lng)))

  private def playerStatus: PlayerStatus = oneOf(PlayerStatus.STOPPED, PlayerStatus.NOT_CONNECTED, PlayerStatus.RUNNING)

  val allComps: Map[SiteType, SiteEntities[SiteCompTrait]] = Map(
    PLAYER -> SiteEntities(COMP, PLAYER, siteComp(templ(PLAYER))
      .map(_.asInstanceOf[SiteComp[PlayerTempl]])
      .map(PlayerComp(_, playerStatus, location))
    ), LAYOUT -> SiteEntities(COMP, LAYOUT, siteComp(templ(LAYOUT))
      .map(_.asInstanceOf[SiteComp[LayoutTempl]])
      .map(LayoutComp(_, oneOf(None, Some(ScreenRegion(fromTop = 40, width = 333, height = 222)))))
    ), REGION -> SiteEntities(COMP, REGION, Seq(
    )), PLAYLIST -> SiteEntities(COMP, PLAYLIST, siteComp(templ(PLAYLIST))
      .map(_.asInstanceOf[SiteComp[PlaylistTempl]])
      .map(PlaylistComp(_))
    ), MEDIUM -> SiteEntities(COMP, MEDIUM, siteComp(templ(MEDIUM))
      .map(_.asInstanceOf[SiteComp[MediumTempl]])
      .map(MediumComp(_))
    ))

  def comp(siteType: SiteType): SiteCompTrait = oneOfSeq(allComps(siteType).entities)

  lazy val siteLevel = SiteLevel(COMP, allComps)

}

object SiteConfCreator {

  import FilterTagCreator._
  import SiteCompCreator._
  import SitesCreator._
  import TimingCreator._

  val entityCount = 20

  private def siteConf(comp: SiteCompTrait): Seq[SiteConfInfo[SiteCompTrait]] = for {i <- 0 to entityCount} yield {
    val ident = siteIdent
    val siteType = comp.siteType
    val label = siteType.label
    SiteConfInfo(ident, Site.nextIdent(ident)
      , comp
      , oneOf(None, None, Some(s"$label 2.3"), Some(s"$i.0 $label"), Some(s"${label.take(2)}-CONF-$i"))
      , oneOf(None, Some(s"Special Config for this $label $i"))
      , oneOf(None, Some(oneOfSeq(filterTagConfs)))
      , oneOf(None, Some(oneOfSeq(timingConfs)))
    )
  }

  private val mediumConfs = SiteEntities(CONF, PLAYER, siteConf(comp(MEDIUM))
    .map(_.asInstanceOf[SiteConfInfo[MediumComp]])
    .map(MediumConf(_)))
  private val playlistConfs = SiteEntities(CONF, PLAYER, siteConf(comp(PLAYLIST))
    .map(_.asInstanceOf[SiteConfInfo[PlaylistComp]])
    .map(PlaylistConf(_, Set(oneOfSeq(mediumConfs.entities), oneOfSeq(mediumConfs.entities), oneOfSeq(mediumConfs.entities)).toSeq))
  )

  private def screenRegion = oneOf(None, None, Some(ScreenRegion(12, 24)))

  private val regionConfs = SiteEntities(CONF, PLAYER, siteConf(comp(LAYOUT))
    .map(_.asInstanceOf[SiteConfInfo[LayoutComp]])
    .map(RegionConf(_, screenRegion, Seq(oneOfSeq(playlistConfs.entities))))
  )
  private val layoutConfs = SiteEntities(CONF, PLAYER, siteConf(comp(LAYOUT))
    .map(_.asInstanceOf[SiteConfInfo[LayoutComp]])
    .map(LayoutConf(_, screenRegion, Seq(oneOfSeq(regionConfs.entities))))
  )
  private val playerConfs = SiteEntities(CONF, PLAYER, siteConf(comp(PLAYER))
    .map(_.asInstanceOf[SiteConfInfo[PlayerComp]])
    .map(c => PlayerConf(c, Seq(oneOfSeq(layoutConfs.entities))))
  )


  val allConfs: Map[SiteType, SiteEntities[_ <: SiteConfTrait]] = Map(
    PLAYER -> playerConfs
    , LAYOUT -> layoutConfs
    , REGION -> regionConfs
    , PLAYLIST -> playlistConfs
    , MEDIUM -> mediumConfs
  )

  lazy val siteLevel = SiteLevel(CONF, allConfs)


}


