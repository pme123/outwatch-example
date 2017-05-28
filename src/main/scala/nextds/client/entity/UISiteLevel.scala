package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary

/**
  * Created by pascal.mengelt on 17.03.2017.
  */
case class UISiteLevel(
                        levelType: LevelType
                        , players: Seq[UIPlayer] = Nil
                        , layouts: Seq[UILayout] = Nil
                        , regions: Seq[UIRegion] = Nil
                        , playlists: Seq[UIPlaylist] = Nil
                        , mediums: Seq[UIMedium] = Nil
                      ) {


  def replaceEntries(updateEntities: UpdateEntities): UISiteLevel =
    updateEntities.siteType match {
      case PLAYER => copy(players = updateEntities.entities.map(_.asInstanceOf[UIPlayer]))
      case LAYOUT => copy(layouts = updateEntities.entities.map(_.asInstanceOf[UILayout]))
      case REGION => copy(regions = updateEntities.entities.map(_.asInstanceOf[UIRegion]))
      case PLAYLIST => copy(playlists = updateEntities.entities.map(_.asInstanceOf[UIPlaylist]))
      case MEDIUM => copy(mediums = updateEntities.entities.map(_.asInstanceOf[UIMedium]))
      case _ => this
    }

  def entities(siteType: SiteType): Seq[UISiteEntity] =
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
        .map(uiEntity)
        .map(_.asInstanceOf[UIPlayer])
      , SiteEntityBoundary.entitiesFor(levelType, LAYOUT)
        .map(uiEntity)
        .map(_.asInstanceOf[UILayout])
      , SiteEntityBoundary.entitiesFor(levelType, REGION)
        .map(uiEntity)
        .map(_.asInstanceOf[UIRegion])
      , SiteEntityBoundary.entitiesFor(levelType, PLAYLIST)
        .map(uiEntity)
        .map(_.asInstanceOf[UIPlaylist])
      , SiteEntityBoundary.entitiesFor(levelType, MEDIUM)
        .map(uiEntity)
        .map(_.asInstanceOf[UIMedium])
    )
}
