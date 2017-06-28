package nextds.server.control

import nextds.entity.{PlaylistConf, _}

// not in use - see SiteEntityCreator
object SitesRepo {

  val publicSite = "PUBLIC"
  val mgaaSite = "MGAA"
  val filtSite = "FILT"
  val timerSite = "TIMER"

  def allSites: Seq[SiteIdent] = {
    Seq(publicSite, mgaaSite, filtSite, timerSite)
  }

  def entitiesFor[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): Seq[T] =
    (levelType match {
      case TEMPL => SiteTemplRepo.allTempls(siteType)
      case COMP => SiteCompRepo.allComps(siteType)
      case CONF => SiteConfRepo.allConfs(siteType)
      case FILTER => Seq()
      case TIME => Seq()
    }).map(_.asInstanceOf[T])

}

object SiteTemplRepo {

  import SitesRepo._
  import nextds.entity.ScreenRegion._

  val playerTempl = PlayerTempl(publicSite, "Web Player 2.0")
  val layoutTempl: LayoutTempl = LayoutTempl.singleLayout(publicSite, "Wide-Screen: Single Layout", fullHD)
  val playlistTempl = PlaylistTempl(SiteEntityInfo(publicSite, "Basic Playlist"))
  val mediumTempl = MediumTempl(SiteEntityInfo(publicSite, "Video"))
  val mediumTempl2 = MediumTempl(SiteEntityInfo(publicSite, "Image"))
  val mediumTempl3 = MediumTempl(SiteEntityInfo(publicSite, "AF"))
  val allTempls: Map[SiteType, Seq[SiteTemplTrait]] = Map(
    PLAYER -> Seq(playerTempl
      , PlayerTempl(SiteEntityInfo(publicSite, Site.nextIdent(publicSite), "Windows Player 2.3", "Special Configs for this Player type"))
    ), LAYOUT -> Seq(layoutTempl
      , LayoutTempl.singleLayout(publicSite, "Basic Wide-Screen", ultraHD4K)
    ), REGION -> Seq(
    ), PLAYLIST -> Seq(
      playlistTempl
    ), MEDIUM -> Seq(
      mediumTempl
      , mediumTempl2
      , mediumTempl3
    )

  )
}

object SiteCompRepo {

  import SitesRepo._
  import SiteTemplRepo._

  val playerComp = PlayerComp(mgaaSite, playerTempl, "Shop-Ville 12f", PlayerStatus.NOT_CONNECTED, PlayerLocation(47.056856, 8.539656700000023))
  val playerComp2 = PlayerComp(filtSite, playerTempl, "Züri-Center", PlayerStatus.RUNNING, PlayerLocation(47.3717306, 8.538627899999938))
  val playerComp3 = PlayerComp(timerSite, playerTempl, "Luzern am Bahnhof", PlayerStatus.STOPPED, PlayerLocation(47.0508225, 8.306212100000039))
  val layoutComp = LayoutComp(mgaaSite, layoutTempl)
  val layoutComp2 = LayoutComp(SiteComp(filtSite, layoutTempl, "Special configuration Layout."))
  val layoutComp3 = LayoutComp(timerSite, layoutTempl)
  val playlistComp = PlaylistComp(SiteComp(mgaaSite, playlistTempl))
  val playlistComp2 = PlaylistComp(SiteComp(filtSite, playlistTempl))
  val playlistComp3 = PlaylistComp(SiteComp(timerSite, playlistTempl))
  val mediumComp = MediumComp(SiteComp(mgaaSite, mediumTempl, "Supervideo.mp4"))
  val mediumComp2 = MediumComp(SiteComp(filtSite, mediumTempl2, "TheVideo.mp4"))
  val mediumComp3 = MediumComp(SiteComp(timerSite, mediumTempl3, "rabbitRuns.mp4"))
  val allComps: Map[SiteType, Seq[SiteCompTrait]] = Map(
    PLAYER -> Seq(
      playerComp, playerComp2, playerComp3
    ), LAYOUT -> Seq(
      layoutComp, layoutComp2, layoutComp3
    ), REGION -> Seq(
    ), PLAYLIST -> Seq(
      playlistComp
    ), MEDIUM -> Seq(
      mediumComp, mediumComp2, mediumComp3
    )

  )
}

object SiteConfRepo {

  import SitesRepo._
  import SiteCompRepo._
  import FilterTagCreator.filterTagConf
  import TimingCreator.timingConf

  private val mediumConfs = Seq(
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
  )

  private val playlistConfs = Seq(
    PlaylistConf(SiteConfInfo(playlistComp), siteConfRefs = mediumConfs.take(4))
    , PlaylistConf(SiteConfInfo(playlistComp2, "Playlist for EN", filterTagConf("EN")), siteConfRefs = mediumConfs)
    , PlaylistConf(SiteConfInfo(playlistComp2, "Playlist for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = mediumConfs)
    , PlaylistConf(SiteConfInfo(playlistComp2, "Playlist for (IT OR EN) AND DE", filterTagConf("(IT OR EN) AND DE")), siteConfRefs = mediumConfs)
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Weekends", timingConf("Weekends")), siteConfRefs = mediumConfs.drop(6))
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Weekdays", timingConf("Weekdays")), siteConfRefs = mediumConfs.drop(6))
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = mediumConfs.drop(6))
    , PlaylistConf(SiteConfInfo(playlistComp3, "Playlist for Christmas Week", timingConf("Christmas Week")), siteConfRefs = mediumConfs.drop(6))
  )
  private val regionConfs = Seq(
    RegionConf(SiteConfInfo(layoutComp), siteConfRefs = playlistConfs.take(4))
    , RegionConf(SiteConfInfo(layoutComp2, "Region for EN", filterTagConf("EN")), siteConfRefs = playlistConfs.take(4))
    , RegionConf(SiteConfInfo(layoutComp2, "Region for DE", filterTagConf("DE")), siteConfRefs = playlistConfs.take(4))
    , RegionConf(SiteConfInfo(layoutComp2, "Region for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = playlistConfs.take(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Weekends", timingConf("Weekends")), siteConfRefs = playlistConfs.drop(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Weekdays", timingConf("Weekdays")), siteConfRefs = playlistConfs.drop(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = playlistConfs.drop(4))
    , RegionConf(SiteConfInfo(layoutComp3, "Region for Christmas Week", timingConf("Christmas Week")), siteConfRefs = playlistConfs.drop(4))
  )
  private val layoutConfs = Seq(
    LayoutConf(mgaaSite, layoutComp, regionConfs = regionConfs)
    , LayoutConf(SiteConfInfo(layoutComp, "Extrem special configuration Layout."), siteConfRefs = regionConfs.take(4))
    , LayoutConf(SiteConfInfo(layoutComp2, "Layout for EN", filterTagConf("EN")), siteConfRefs = regionConfs.take(4))
    , LayoutConf(SiteConfInfo(layoutComp2, "Layout for DE", filterTagConf("DE")), siteConfRefs = regionConfs.take(4))
    , LayoutConf(SiteConfInfo(layoutComp2, "Layout for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = regionConfs.take(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Weekends", timingConf("Weekends")), siteConfRefs = regionConfs.drop(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Weekdays", timingConf("Weekdays")), siteConfRefs = regionConfs.drop(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = regionConfs.drop(4))
    , LayoutConf(SiteConfInfo(layoutComp3, "Layout for Christmas Week", timingConf("Christmas Week")), siteConfRefs = regionConfs.drop(4))

  )
  private val playerConfs = Seq(
    PlayerConf(mgaaSite, playerComp, layoutConfs)
    , PlayerConf(mgaaSite, playerComp, layoutConfs)
    , PlayerConf(mgaaSite, playerComp, layoutConfs)
    , PlayerConf(SiteConfInfo(playerComp2, "Player for EN", filterTagConf("EN")), siteConfRefs = layoutConfs.take(4))
    , PlayerConf(SiteConfInfo(playerComp2, "Player for DE", filterTagConf("DE")), siteConfRefs = layoutConfs.take(4))
    , PlayerConf(SiteConfInfo(playerComp2, "Player for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = layoutConfs.take(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Weekends", timingConf("Weekends")), siteConfRefs = layoutConfs.drop(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Weekdays", timingConf("Weekdays")), siteConfRefs = layoutConfs.drop(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Monday and Friday", timingConf("Monday and Friday")), siteConfRefs = layoutConfs.drop(4))
    , PlayerConf(SiteConfInfo(playerComp3, "Player for Christmas Week", timingConf("Christmas Week")), siteConfRefs = layoutConfs.drop(4))
  )

  val allConfs: Map[SiteType, Seq[SiteConfTrait]] = Map(
    PLAYER -> playerConfs
    , LAYOUT -> layoutConfs
    , PLAYLIST -> playlistConfs
    , REGION -> regionConfs
    , MEDIUM -> mediumConfs

  )
}
