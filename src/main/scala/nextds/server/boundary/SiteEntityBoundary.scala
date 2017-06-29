package nextds.server.boundary

import nextds.entity._
import nextds.server.control._

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
object SiteEntityBoundary {

  def allSites(): Seq[SiteIdent] = SitesRepo.allSites ++ SitesCreator.allSites

  def siteIdent(): SiteIdent = allSites()(1)

  def siteModel(): SiteModel = SiteModel(SitesRepo.siteLevels :+ FilterTagCreator.siteLevel :+ TimingCreator.siteLevel: _*)

  def entitiesFor(levelType: LevelType, siteType: SiteType): Seq[SiteEntityTrait] =
    SitesRepo.entitiesFor(levelType, siteType).entities ++ SitesCreator.entitiesFor(levelType, siteType).entities

  def createFrom(siteEntityTrait: SiteEntityTrait, siteIdent: String, isForRegion: Boolean = false): SiteEntityTrait = siteEntityTrait.levelType match {
    case TEMPL =>
      siteEntityTrait.siteType match {
        case PLAYER => PlayerComp(siteIdent, siteEntityTrait.asInstanceOf[PlayerTempl])
        case LAYOUT => LayoutComp(siteIdent, siteEntityTrait.asInstanceOf[LayoutTempl])
        case PLAYLIST => PlaylistComp(siteIdent, siteEntityTrait.asInstanceOf[PlaylistTempl])
        case MEDIUM => MediumComp(siteIdent, siteEntityTrait.asInstanceOf[MediumTempl])
        case other => throw new IllegalArgumentException(s"Not supported: $other!")

      }
    case COMP =>
      siteEntityTrait.siteType match {
        case PLAYER => PlayerConf(siteIdent, siteEntityTrait.asInstanceOf[PlayerComp])
        case LAYOUT if isForRegion => RegionConf(siteIdent, siteEntityTrait.asInstanceOf[LayoutComp])
        case LAYOUT => LayoutConf(siteIdent, siteEntityTrait.asInstanceOf[LayoutComp])
        case PLAYLIST => PlaylistConf(siteIdent, siteEntityTrait.asInstanceOf[PlaylistComp])
        case MEDIUM => MediumConf(siteIdent, siteEntityTrait.asInstanceOf[MediumComp])
        case other => throw new IllegalArgumentException(s"Not supported: $other!")

      }
    case other => throw new IllegalArgumentException(s"Not supported: $other!")
  }
}
