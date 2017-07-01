package nextds.server.control

import nextds.entity.{COMP, CONF, PlaylistConf, TEMPL, _}

// not in use - see SiteEntityCreator
object SitesRepo {

  val publicSite = "PUBLIC"
  val mgaaSite = "MGAA"
  val filtSite = "FILT"
  val timerSite = "TIMER"

  def allSites: Seq[SiteIdent] = {
    Seq(publicSite, mgaaSite, filtSite, timerSite)
  }

  def entitiesFor(levelType: LevelType, siteType: SiteType): SiteEntities[_ <: SiteEntityTrait] =
    levelType match {
      case TEMPL => SiteTemplRepo.allTempls(siteType)
      case COMP => SiteCompRepo.allComps(siteType)
      case CONF => SiteConfRepo.allConfs(siteType)
      case FILTER | TIME => SiteEntities(levelType, siteType, Nil)
    }

  lazy val siteLevels: Seq[SiteLevel] =
    Seq(
      SiteTemplRepo.siteLevel
      , SiteCompRepo.siteLevel
      , SiteConfRepo.siteLevel
    )

}

object SiteTemplRepo {

  import SitesRepo._
  import nextds.entity.ScreenRegion._

  val playerTempl = PlayerTempl(publicSite, "Web Player 2.0")
  val layoutWidescreenTempl: LayoutTempl = LayoutTempl.singleLayout(publicSite, "Wide-Screen: Single Layout", fullHD)
  val layoutUltraTempl: LayoutTempl = LayoutTempl.singleLayout(publicSite, "Basic Wide-Screen", ultraHD4K)
  val playlistTempl = PlaylistTempl(SiteEntityInfo(publicSite, "Basic Playlist"))
  val mediumTempl = MediumTempl(SiteEntityInfo(publicSite, "Video"))
  val mediumTempl2 = MediumTempl(SiteEntityInfo(publicSite, "Image"))
  val mediumTempl3 = MediumTempl(SiteEntityInfo(publicSite, "AF"))
  val allTempls: Map[SiteType, SiteEntities[_ <: SiteEntityTrait]] = Map(
    PLAYER -> SiteEntities(TEMPL, PLAYER, Seq(playerTempl
      , PlayerTempl(SiteEntityInfo(publicSite, Site.nextIdent(publicSite), "Windows Player 2.3", "Special Configs for this Player type"))
    )), LAYOUT -> SiteEntities(TEMPL, LAYOUT, Seq(layoutWidescreenTempl
      , layoutUltraTempl
    )), REGION -> SiteEntities(TEMPL, REGION, Seq(
    )), PLAYLIST -> SiteEntities(TEMPL, PLAYLIST, Seq(
      playlistTempl
    )), MEDIUM -> SiteEntities(TEMPL, MEDIUM, Seq(
      mediumTempl
      , mediumTempl2
      , mediumTempl3
    ))
  )

  lazy val siteLevel = SiteLevel(TEMPL, allTempls)

}

object SiteCompRepo {

  import SitesRepo._
  import SiteTemplRepo._

  val playerComp = PlayerComp(mgaaSite, playerTempl, "Shop-Ville 12f", PlayerStatus.NOT_CONNECTED, PlayerLocation(47.056856, 8.539656700000023))
  val playerComp2 = PlayerComp(filtSite, playerTempl, "Züri-Center", PlayerStatus.RUNNING, PlayerLocation(47.3717306, 8.538627899999938))
  val playerComp3 = PlayerComp(timerSite, playerTempl, "Luzern am Bahnhof", PlayerStatus.STOPPED, PlayerLocation(47.0508225, 8.306212100000039))
  val layoutComp = LayoutComp(mgaaSite, layoutWidescreenTempl)
  val layoutComp2 = LayoutComp(SiteComp(filtSite, layoutWidescreenTempl, "Special configuration Layout."), Some(ScreenRegion(100, 100, 200, 200)))
  val layoutComp3 = LayoutComp(timerSite, layoutUltraTempl)
  val playlistComp = PlaylistComp(SiteComp(mgaaSite, playlistTempl))
  val playlistComp2 = PlaylistComp(SiteComp(filtSite, playlistTempl))
  val playlistComp3 = PlaylistComp(SiteComp(timerSite, playlistTempl))
  val mediumComp = MediumComp(mgaaSite, "SuperBOY.png", mediumTempl2)
  val mediumComp2 = MediumComp(mgaaSite, "Meatballs.png", mediumTempl2)
  val mediumComp3 = MediumComp(mgaaSite, "Fruits.png", mediumTempl2)
  val allComps: Map[SiteType, SiteEntities[_ <: SiteEntityTrait]] = Map(
    PLAYER -> SiteEntities(COMP, PLAYER, Seq(
      playerComp, playerComp2, playerComp3
    )), LAYOUT -> SiteEntities(COMP, LAYOUT, Seq(
      layoutComp, layoutComp2, layoutComp3
    )), REGION -> SiteEntities(COMP, REGION, Seq(
    )), PLAYLIST -> SiteEntities(COMP, PLAYLIST, Seq(
      playlistComp
    )), MEDIUM -> SiteEntities(COMP, MEDIUM, Seq(
      mediumComp, mediumComp2, mediumComp3
    ))
  )
  lazy val siteLevel = SiteLevel(COMP, allComps)
}

object SiteConfRepo {

  import SitesRepo._
  import SiteCompRepo._
  import FilterTagCreator.filterTagConf
  import TimingCreator.timingConf

  private val mediumConfs = SiteEntities[MediumConf](CONF, MEDIUM, Seq(
    MediumConf(SiteConfInfo(mediumComp))
    , MediumConf(SiteConfInfo(mediumComp2))
    , MediumConf(SiteConfInfo(mediumComp3))
    , MediumConf(SiteConfInfo(mediumComp2, "Medium for DE", filterTagConf("DE")))
    , MediumConf(SiteConfInfo(mediumComp2, "Medium for EN", filterTagConf("EN")))
    , MediumConf(SiteConfInfo(mediumComp2, "Medium for IT", filterTagConf("IT")))
    , MediumConf(SiteConfInfo(mediumComp2, "Medium for EN OR DE", filterTagConf("EN OR DE")))
    , MediumConf(SiteConfInfo(mediumComp2, "Medium for EN AND DE", filterTagConf("EN AND DE")))
    , MediumConf(SiteConfInfo(mediumComp3, "Medium for Weekends", timingConf("Weekends")))
    , MediumConf(SiteConfInfo(mediumComp3, "Medium for Weekdays", timingConf("Weekdays")))
    , MediumConf(SiteConfInfo(mediumComp3, "Medium for Monday and Friday", timingConf("Monday and Friday")))
    , MediumConf(SiteConfInfo(mediumComp3, "Medium for Opening Hours", timingConf("Opening Hours")))
    , MediumConf(SiteConfInfo(mediumComp3, "Medium for Christmas Week", timingConf("Christmas Week")))
  ))

  private val playlistConfs = SiteEntities[PlaylistConf](CONF, PLAYLIST, Seq(
    PlaylistConf(SiteConfInfo(playlistComp), siteConfRefs = mediumConfs.entities.take(4))
    , PlaylistConf(SiteConfInfo(playlistComp2, "Playlist for EN", filterTagConf("EN")), siteConfRefs = mediumConfs.entities)
    , PlaylistConf(SiteConfInfo(playlistComp2, "Playlist for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = mediumConfs.entities)
    , PlaylistConf(SiteConfInfo(playlistComp2, "Playlist for (IT OR EN) AND DE", filterTagConf("(IT OR EN) AND DE")), siteConfRefs = mediumConfs.entities)
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Weekends", timingConf("Weekends")), siteConfRefs = mediumConfs.entities.drop(6))
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Weekdays", timingConf("Weekdays")), siteConfRefs = mediumConfs.entities.drop(6))
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = mediumConfs.entities.drop(6))
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Christmas Week", timingConf("Christmas Week")), siteConfRefs = mediumConfs.entities.drop(6))
  ))
  private val regionConfs = SiteEntities[RegionConf](CONF, REGION, Seq(
    RegionConf(SiteConfInfo(layoutComp), siteConfRefs = playlistConfs.entities.take(1))
    , RegionConf(SiteConfInfo(layoutComp2, "Region for EN", filterTagConf("EN")), siteConfRefs = playlistConfs.entities.take(4))
    , RegionConf(SiteConfInfo(layoutComp2, "Region for DE", filterTagConf("DE")), siteConfRefs = playlistConfs.entities.take(4))
    , RegionConf(SiteConfInfo(layoutComp2, "Region for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = playlistConfs.entities.take(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Weekends", timingConf("Weekends")), siteConfRefs = playlistConfs.entities.drop(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Weekdays", timingConf("Weekdays")), siteConfRefs = playlistConfs.entities.drop(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = playlistConfs.entities.drop(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Christmas Week", timingConf("Christmas Week")), siteConfRefs = playlistConfs.entities.drop(4))
  ))
  private val layoutConfs = SiteEntities[LayoutConf](CONF, LAYOUT, Seq(
    LayoutConf(mgaaSite, layoutComp, regionConfs = regionConfs.entities.take(2))
    , LayoutConf(SiteConfInfo(layoutComp, "Extrem special configuration Layout."), siteConfRefs = regionConfs.entities.take(4))
    , LayoutConf(SiteConfInfo(layoutComp2, "Layout for EN", filterTagConf("EN")), siteConfRefs = regionConfs.entities.take(4))
    , LayoutConf(SiteConfInfo(layoutComp2, "Layout for DE", filterTagConf("DE")), siteConfRefs = regionConfs.entities.take(4))
    , LayoutConf(SiteConfInfo(layoutComp2, "Layout for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = regionConfs.entities.take(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Weekends", timingConf("Weekends")), siteConfRefs = regionConfs.entities.drop(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Weekdays", timingConf("Weekdays")), siteConfRefs = regionConfs.entities.drop(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = regionConfs.entities.drop(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Christmas Week", timingConf("Christmas Week")), siteConfRefs = regionConfs.entities.drop(4))

  ))
  private val playerConfs = SiteEntities[PlayerConf](CONF, PLAYER, Seq(
    PlayerConf(mgaaSite, playerComp, layoutConfs.entities)
    , PlayerConf(mgaaSite, playerComp, layoutConfs.entities)
    , PlayerConf(mgaaSite, playerComp, layoutConfs.entities)
    , PlayerConf(SiteConfInfo(playerComp2, "Player for EN", filterTagConf("EN")), siteConfRefs = layoutConfs.entities.take(4))
    , PlayerConf(SiteConfInfo(playerComp2, "Player for DE", filterTagConf("DE")), siteConfRefs = layoutConfs.entities.take(4))
    , PlayerConf(SiteConfInfo(playerComp2, "Player for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = layoutConfs.entities.take(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Weekends", timingConf("Weekends")), siteConfRefs = layoutConfs.entities.drop(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Weekdays", timingConf("Weekdays")), siteConfRefs = layoutConfs.entities.drop(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = layoutConfs.entities.drop(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Christmas Week", timingConf("Christmas Week")), siteConfRefs = layoutConfs.entities.drop(4))
  ))

  val allConfs: Map[SiteType, SiteEntities[_ <: SiteEntityTrait]] = Map(
    PLAYER -> playerConfs
    , LAYOUT -> layoutConfs
    , PLAYLIST -> playlistConfs
    , REGION -> regionConfs
    , MEDIUM -> mediumConfs

  )

  lazy val siteLevel = SiteLevel(CONF, allConfs)

}
