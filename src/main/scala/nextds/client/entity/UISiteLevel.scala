package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary

/**
  * Created by pascal.mengelt on 17.03.2017.
  */
case class UISiteLevel(
                        levelType: LevelType
                        , players: Seq[PlayerTrait] = Nil
                        , layouts: Seq[LayoutTrait] = Nil
                        , regions: Seq[LayoutTrait] = Nil
                        , playlists: Seq[PlaylistTrait] = Nil
                        , mediums: Seq[MediumTrait] = Nil
                      ) {


  def replaceEntries(updateEntities: UpdateEntities): UISiteLevel =
    updateEntities.siteType match {
      case PLAYER => copy(players = updateEntities.entities.map(_.asInstanceOf[PlayerTrait]))
      case LAYOUT => copy(layouts = updateEntities.entities.map(_.asInstanceOf[LayoutTrait]))
      case REGION => copy(regions = updateEntities.entities.map(_.asInstanceOf[LayoutTrait]))
      case PLAYLIST => copy(playlists = updateEntities.entities.map(_.asInstanceOf[PlaylistTrait]))
      case MEDIUM => copy(mediums = updateEntities.entities.map(_.asInstanceOf[MediumTrait]))
      case _ => this
    }

  def entities(siteType: SiteType): Seq[SiteEntityTrait] =
    siteType match {
      case PLAYER => players
      case LAYOUT => layouts
      case REGION => regions
      case PLAYLIST => playlists
      case MEDIUM => mediums
      case _ => Nil
    }
  def allSiteTypes: Seq[SiteType] = Seq(PLAYER, LAYOUT, REGION, PLAYLIST, MEDIUM)

}

object UISiteLevel {
  def apply(levelType: LevelType): UISiteLevel =
    new UISiteLevel(
      levelType
      , SiteEntityBoundary.entitiesFor(levelType, PLAYER)
      , SiteEntityBoundary.entitiesFor(levelType, LAYOUT)
      , SiteEntityBoundary.entitiesFor(levelType, REGION)
      , SiteEntityBoundary.entitiesFor(levelType, PLAYLIST)
      , SiteEntityBoundary.entitiesFor(levelType, MEDIUM))
}
