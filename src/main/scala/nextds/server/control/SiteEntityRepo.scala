package nextds.server.control

import nextds.entity.{PlaylistConf, _}

// not in use - see SiteEntityCreator
object SitesRepo {
  def allSites(): Seq[SiteIdent] = Seq("PUBLIC", "MGAA")

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

  import nextds.entity.ScreenRegion._

  val defaultSiteIdent: SiteIdent = SitesRepo.allSites().head

  val playerTempl = PlayerTempl(defaultSiteIdent, "Web Player 2.0")
  val layoutTempl: LayoutTempl = LayoutTempl.singleLayout(defaultSiteIdent, "Wide-Screen: Single Layout", fullHD)
  val playlistTempl = PlaylistTempl(SiteEntityInfo(defaultSiteIdent, "Basic Playlist"))
  val mediumTempl = MediumTempl(SiteEntityInfo(defaultSiteIdent, "Video"))
  val mediumTempl2 = MediumTempl(SiteEntityInfo(defaultSiteIdent, "Image"))
  val mediumTempl3 = MediumTempl(SiteEntityInfo(defaultSiteIdent, "AF"))
  val allTempls: Map[SiteType, Seq[SiteTemplTrait]] = Map(
    PLAYER -> Seq(playerTempl
      , PlayerTempl(SiteEntityInfo(defaultSiteIdent, Site.nextIdent(defaultSiteIdent), "Windows Player 2.3", "Special Configs for this Player type"))
    ), LAYOUT -> Seq(layoutTempl
      , LayoutTempl.singleLayout(defaultSiteIdent, "Basic Wide-Screen", ultraHD4K)
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

  import SiteTemplRepo._

  val defaultSiteIdent: SiteIdent = SitesRepo.allSites()(1)

  val playerComp = PlayerComp(defaultSiteIdent, playerTempl, "Shop-Ville 12f", PlayerStatus.NOT_CONNECTED, PlayerLocation(47.056856, 8.539656700000023))
  val playerComp2 = PlayerComp(defaultSiteIdent, playerTempl, "Züri-Center", PlayerStatus.RUNNING, PlayerLocation(47.3717306, 8.538627899999938))
  val playerComp3 = PlayerComp(defaultSiteIdent, playerTempl, "Luzern am Bahnhof", PlayerStatus.STOPPED, PlayerLocation(47.0508225, 8.306212100000039))
  val layoutComp = LayoutComp(defaultSiteIdent, layoutTempl)
  val layoutComp2 = LayoutComp(SiteComp(defaultSiteIdent, layoutTempl, "Special configuration Layout."))
  val playlistComp = PlaylistComp(SiteComp(defaultSiteIdent, playlistTempl))
  val mediumComp = MediumComp(SiteComp(defaultSiteIdent, mediumTempl, "Supervideo.mp4"))
  val mediumComp2 = MediumComp(SiteComp(defaultSiteIdent, mediumTempl2, "TheVideo.mp4"))
  val mediumComp3 = MediumComp(SiteComp(defaultSiteIdent, mediumTempl3, "rabbitRuns.mp4"))
  val allComps: Map[SiteType, Seq[SiteCompTrait]] = Map(
    PLAYER -> Seq(
      playerComp, playerComp2, playerComp3
    ), LAYOUT -> Seq(
      layoutComp, layoutComp2
    ), REGION -> Seq(
    ), PLAYLIST -> Seq(
      playlistComp
    ), MEDIUM -> Seq(
      mediumComp, mediumComp3, mediumComp3
    )

  )
}

object SiteConfRepo {

  import SiteCompRepo._
  import FilterTagCreator.filterTagConf

  val defaultSiteIdent: SiteIdent = SitesRepo.allSites()(1)

  private val mediumConfs = Seq(
    MediumConf(SiteConfInfo(mediumComp))
    , MediumConf(SiteConfInfo(mediumComp2))
    , MediumConf(SiteConfInfo(mediumComp3))
    , MediumConf(SiteConfInfo(mediumComp, "Medium for DE", filterTagConf("DE")))
    , MediumConf(SiteConfInfo(mediumComp, "Medium for EN", filterTagConf("EN")))
    , MediumConf(SiteConfInfo(mediumComp, "Medium for IT", filterTagConf("IT")))
    , MediumConf(SiteConfInfo(mediumComp, "Medium for EN OR DE", filterTagConf("EN OR DE")))
    , MediumConf(SiteConfInfo(mediumComp, "Medium for EN AND DE", filterTagConf("EN AND DE")))
  )
  private val playlistConfs = Seq(
    PlaylistConf(SiteConfInfo(playlistComp), siteConfRefs = mediumConfs.take(4))
    , PlaylistConf(SiteConfInfo(playlistComp, "Playlist for EN", filterTagConf("EN")), siteConfRefs = mediumConfs)
    , PlaylistConf(SiteConfInfo(playlistComp, "Playlist for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = mediumConfs)
    , PlaylistConf(SiteConfInfo(playlistComp, "Playlist for (IT OR EN) AND DE", filterTagConf("(IT OR EN) AND DE")), siteConfRefs = mediumConfs)
  )
  private val regionConfs = Seq(
    RegionConf(SiteConfInfo(layoutComp), siteConfRefs = playlistConfs)
    , RegionConf(SiteConfInfo(layoutComp, "Region for EN", filterTagConf("EN")), siteConfRefs = playlistConfs)
    , RegionConf(SiteConfInfo(layoutComp, "Region for DE", filterTagConf("DE")), siteConfRefs = playlistConfs)
    , RegionConf(SiteConfInfo(layoutComp, "Region for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = playlistConfs)
  )
  private val layoutConfs = Seq(
    LayoutConf(defaultSiteIdent, layoutComp, regionConfs = regionConfs)
    , LayoutConf(SiteConfInfo(layoutComp2, "Extrem special configuration Layout."), siteConfRefs = regionConfs)
    , LayoutConf(SiteConfInfo(layoutComp, "Layout for EN", filterTagConf("EN")), siteConfRefs = regionConfs)
    , LayoutConf(SiteConfInfo(layoutComp, "Layout for DE", filterTagConf("DE")), siteConfRefs = regionConfs)
    , LayoutConf(SiteConfInfo(layoutComp, "Layout for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = regionConfs)
  )
  private val playerConfs = Seq(
    PlayerConf(defaultSiteIdent, playerComp, layoutConfs)
    , PlayerConf(defaultSiteIdent, playerComp2, layoutConfs)
    , PlayerConf(defaultSiteIdent, playerComp3, layoutConfs)
    , PlayerConf(SiteConfInfo(playerComp, "Player for EN", filterTagConf("EN")), siteConfRefs = layoutConfs)
    , PlayerConf(SiteConfInfo(playerComp, "Player for DE", filterTagConf("DE")), siteConfRefs = layoutConfs)
    , PlayerConf(SiteConfInfo(playerComp, "Player for EN OR DE", filterTagConf("EN OR DE")), siteConfRefs = layoutConfs)
  )

  val allConfs: Map[SiteType, Seq[SiteConfTrait]] = Map(
    PLAYER -> playerConfs
    , LAYOUT -> layoutConfs
    , PLAYLIST -> playlistConfs
    , REGION -> regionConfs
    , MEDIUM -> mediumConfs

  )
}
