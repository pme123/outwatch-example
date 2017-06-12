package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary

/**
  * Created by pascal.mengelt on 17.03.2017.
  */
trait UISiteLevelTrait {

  def levelType: LevelType

  def entities(siteType: SiteType): Seq[UISiteEntity]

  def replaceEntries(updateEntities: UpdateEntities): UISiteLevelTrait

  def entity(siteType: SiteType, indexFrom: Int): UISiteEntity

  def allSiteTypes: Seq[SiteType]

}

case class UISiteLevel(
                        levelType: LevelType
                        , players: Seq[UIPlayer] = Nil
                        , layouts: Seq[UILayout] = Nil
                        , regions: Seq[UIRegion] = Nil
                        , playlists: Seq[UIPlaylist] = Nil
                        , mediums: Seq[UIMedium] = Nil
                      )
  extends UISiteLevelTrait {


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
      case other => throw new IllegalArgumentException(s"Unsupported Type: $other")
    }

  def entity(siteType: SiteType, indexFrom: Int): UISiteEntity =
    siteType match {
      case PLAYER => players(indexFrom)
      case LAYOUT => layouts(indexFrom)
      case REGION => regions(indexFrom)
      case PLAYLIST => playlists(indexFrom)
      case MEDIUM => mediums(indexFrom)
      case other => throw new IllegalArgumentException(s"Unsupported Type: $other")
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

case class UIFilterLevel(
                          uiTagFilters: Seq[UITagFilter] = Nil
                          , uiTimeFilters: Seq[UITimeFilter] = Nil
                        )
  extends UISiteLevelTrait {
  val levelType: LevelType = FILTER

  def replaceEntries(updateEntities: UpdateEntities): UIFilterLevel =
    updateEntities.siteType match {
      case TAG_FILTER => copy(uiTagFilters = updateEntities.entities.map(_.asInstanceOf[UITagFilter]))
      case TIME_FILTER => copy(uiTimeFilters = updateEntities.entities.map(_.asInstanceOf[UITimeFilter]))
      case _ => this
    }

  def entities(siteType: SiteType): Seq[UISiteEntity] =
    siteType match {
      case TAG_FILTER => uiTagFilters
      case TIME_FILTER => uiTimeFilters
      case other => throw new IllegalArgumentException(s"Unsupported Type: $other")
    }

  def entity(siteType: SiteType, indexFrom: Int): UISiteEntity =
    siteType match {
      case TAG_FILTER => uiTagFilters(indexFrom)
      case TIME_FILTER => uiTimeFilters(indexFrom)
      case other => throw new IllegalArgumentException(s"Unsupported Type: $other")
    }

  def allSiteTypes: Seq[SiteType] = Seq(TAG_FILTER, TIME_FILTER)

}

object UIFilterLevel {
  def apply(): UIFilterLevel =
    new UIFilterLevel(
      SiteEntityBoundary.entitiesFor(FILTER, TAG_FILTER)
        .map(uiEntity)
        .map(_.asInstanceOf[UITagFilter])
      , SiteEntityBoundary.entitiesFor(FILTER, TIME_FILTER)
        .map(uiEntity)
        .map(_.asInstanceOf[UITimeFilter])
    )
}
