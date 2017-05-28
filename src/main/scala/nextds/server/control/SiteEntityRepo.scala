package nextds.server.control

import nextds.entity._

object SiteTemplRepo {

  def entitiesFor[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): Seq[T] =
    (levelType match {
      case TEMPL => SiteTemplRepo.allTempls(siteType)
      case COMP => SiteCompRepo.allComps(siteType)
      case CONF => SiteConfRepo.allConfs(siteType)
      case _ => //TODO
        SiteTemplRepo.allTempls(siteType)
    }).map(_.asInstanceOf[T])

  import nextds.entity.ScreenRegion._

  val defaultSiteId = "PUBLIC"

  val playerTempl = PlayerTempl(defaultSiteId, "Web Player 2.0")
  val layoutTempl: LayoutTempl = LayoutTempl.singleLayout(defaultSiteId, "Wide-Screen: Single Layout", fullHD)
  val playlistTempl = PlaylistTempl(SiteTempl(defaultSiteId, "Basic Playlist"))
  val mediumTempl = MediumTempl(SiteTempl(defaultSiteId, "Video"))
  val mediumTempl2 = MediumTempl(SiteTempl(defaultSiteId, "Image"))
  val mediumTempl3 = MediumTempl(SiteTempl(defaultSiteId, "AF"))
  val allTempls: Map[SiteType, Seq[SiteTemplTrait]] = Map(
    PLAYER -> Seq(playerTempl
      , PlayerTempl(SiteTempl(defaultSiteId, "Windows Player 2.3", "Special Configs for this Player type"))
    ), LAYOUT -> Seq(layoutTempl
      , LayoutTempl.singleLayout(defaultSiteId, "Basic Wide-Screen", ultraHD4K)
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

  val defaultSiteId = "MGAA"

  val playerComp = PlayerComp(defaultSiteId, playerTempl, "Shop-Ville 12f", PlayerStatus.NOT_CONNECTED, PlayerLocation(47.056856, 8.539656700000023))
  val playerComp2 = PlayerComp(defaultSiteId, playerTempl, "Züri-Center", PlayerStatus.RUNNING, PlayerLocation(47.3717306, 8.538627899999938))
  val playerComp3 = PlayerComp(defaultSiteId, playerTempl, "Luzern am Bahnhof", PlayerStatus.STOPPED, PlayerLocation(47.0508225, 8.306212100000039))
  val layoutComp = LayoutComp(defaultSiteId, layoutTempl)
  val layoutComp2 = LayoutComp(SiteComp(defaultSiteId, layoutTempl, Some("Special configuration Layout.")))
  val playlistComp = PlaylistComp(SiteComp(defaultSiteId, playlistTempl))
  val mediumComp = MediumComp(SiteComp(defaultSiteId, mediumTempl, Some("Supervideo.mp4")))
  val mediumComp2 = MediumComp(SiteComp(defaultSiteId, mediumTempl2, Some("TheVideo.mp4")))
  val mediumComp3 = MediumComp(SiteComp(defaultSiteId, mediumTempl3, Some("rabbitRuns.mp4")))
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

  val defaultSiteId = "MGAA"

  val allConfs: Map[SiteType, Seq[SiteConfTrait]] = Map(
    PLAYER -> Seq(
      PlayerConf(defaultSiteId, playerComp)
      , PlayerConf(defaultSiteId, playerComp2)
      , PlayerConf(defaultSiteId, playerComp3)
    ), LAYOUT -> Seq(
      LayoutConf(defaultSiteId, layoutComp)
      , LayoutConf(SiteConf(defaultSiteId, layoutComp2, Some("Extrem special configuration Layout.")))
    ), PLAYLIST -> Seq(
      PlaylistConf(SiteConf(defaultSiteId, playlistComp))
    ), REGION -> Seq(
      RegionConf(SiteConf(defaultSiteId, layoutComp))
    ), MEDIUM -> Seq(
      MediumConf(SiteConf(defaultSiteId, mediumComp))
      , MediumConf(SiteConf(defaultSiteId, mediumComp2))
      , MediumConf(SiteConf(defaultSiteId, mediumComp3))
    )

  )
}

