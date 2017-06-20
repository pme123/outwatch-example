package nextds.server.control

import nextds.entity._
object SitesRepo {
  def allSites(): Seq[SiteIdent] = Seq("PUBLIC", "MGAA")

}
object SiteTemplRepo {

  def entitiesFor[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): Seq[T] =
    (levelType match {
      case TEMPL => SiteTemplRepo.allTempls(siteType)
      case COMP => SiteCompRepo.allComps(siteType)
      case CONF => SiteConfRepo.allConfs(siteType)
      case FILTER => SiteFilterRepo.allConfs(siteType)
      case _ => //TODO
        SiteTemplRepo.allTempls(siteType)
    }).map(_.asInstanceOf[T])

  import nextds.entity.ScreenRegion._

  val defaultSiteIdent: SiteIdent = SitesRepo.allSites().head

  val playerTempl = PlayerTempl(defaultSiteIdent, "Web Player 2.0")
  val layoutTempl: LayoutTempl = LayoutTempl.singleLayout(defaultSiteIdent, "Wide-Screen: Single Layout", fullHD)
  val playlistTempl = PlaylistTempl(SiteTempl(defaultSiteIdent, "Basic Playlist"))
  val mediumTempl = MediumTempl(SiteTempl(defaultSiteIdent, "Video"))
  val mediumTempl2 = MediumTempl(SiteTempl(defaultSiteIdent, "Image"))
  val mediumTempl3 = MediumTempl(SiteTempl(defaultSiteIdent, "AF"))
  val allTempls: Map[SiteType, Seq[SiteTemplTrait]] = Map(
    PLAYER -> Seq(playerTempl
      , PlayerTempl(SiteTempl(defaultSiteIdent, Site.nextIdent(defaultSiteIdent),"Windows Player 2.3", "Special Configs for this Player type"))
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

  val defaultSiteIdent: SiteIdent = SitesRepo.allSites()(1)

  private val mediumConfs = Seq(
    MediumConf(SiteConf(mediumComp))
    , MediumConf(SiteConf( mediumComp2))
    , MediumConf(SiteConf( mediumComp3))
  )
  private val playlistConfs = Seq(
    PlaylistConf(SiteConf( playlistComp), siteConfRefs = mediumConfs)
  )
  private val regionConfs = Seq(
    RegionConf(SiteConf( layoutComp), siteConfRefs = playlistConfs)
  )
  private val layoutConfs = Seq(
    LayoutConf(defaultSiteIdent, layoutComp, regionConfs = regionConfs)
    , LayoutConf(SiteConf(layoutComp2, "Extrem special configuration Layout."), siteConfRefs = regionConfs)
  )
  private val playerConfs = Seq(
    PlayerConf(defaultSiteIdent, playerComp, layoutConfs)
    , PlayerConf(defaultSiteIdent, playerComp2)
    , PlayerConf(defaultSiteIdent, playerComp3)
  )

  val allConfs: Map[SiteType, Seq[SiteConfTrait]] = Map(
    PLAYER -> playerConfs
    , LAYOUT -> layoutConfs
    , PLAYLIST -> playlistConfs
    , REGION -> regionConfs
    , MEDIUM -> mediumConfs

  )
}

object SiteFilterRepo {

  val publicSiteIdent: SiteIdent = SitesRepo.allSites().head
  val defaultSiteIdent: SiteIdent = SitesRepo.allSites()(0)

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

