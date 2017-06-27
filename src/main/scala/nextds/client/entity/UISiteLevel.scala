package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary

/**
  * Created by pascal.mengelt on 17.03.2017.
  */
trait UISiteLevelTrait {


  def levelType: LevelType

  def entities(siteType: SiteType): Seq[UISiteEntity] =
    siteEntities(siteType)

  def entity(siteType: SiteType, indexFrom: Int): UISiteEntity =
    siteEntities(siteType)(indexFrom)

  def entity(siteType: SiteType, ident: String): UISiteEntity =
    siteEntities(siteType)
      .find(_.siteEntity.ident == ident)
      .getOrElse(throw new IllegalArgumentException(s"There is no SiteEntity with ident: $ident"))


  def replaceEntity(uiSiteEntity: UISiteEntity): UISiteLevelTrait =
    replaceAllEntries(siteEntities.updated(uiSiteEntity.siteType
      , siteEntities(uiSiteEntity.siteType)
        .map {
          case se if se.ident == uiSiteEntity.ident =>
            uiSiteEntity
          case se => se
        }
    ))

  def replaceEntries(updateEntities: UpdateEntities): UISiteLevelTrait =
    replaceAllEntries(siteEntities.updated(updateEntities.siteType
      , updateEntities.entities))

  def siteEntities: Map[SiteType, Seq[UISiteEntity]]

  def replaceAllEntries(siteEntities: Map[SiteType, Seq[UISiteEntity]]): UISiteLevelTrait

  def allSiteTypes: Seq[SiteType]

  def appendFilter(filters: UIFilters): UISiteLevelTrait = {
    filter(siteEntities.map {
      case (k, v) =>
        k -> v.map(_.appendFilter(filters))
    })
  }

  protected def filter(siteEntities: Map[SiteType, Seq[UISiteEntity]]): UISiteLevelTrait

}

case class UISiteLevel(
                        levelType: LevelType
                        , siteEntities: Map[SiteType, Seq[UISiteEntity]] = Map()
                        , isFiltered: Boolean = false)
  extends UISiteLevelTrait {


  def allSiteTypes: Seq[SiteType] = Seq(PLAYER, LAYOUT, REGION, PLAYLIST, MEDIUM)

  def replaceAllEntries(siteEntities: Map[SiteType, Seq[UISiteEntity]]): UISiteLevelTrait =
    copy(siteEntities = siteEntities)

  protected def filter(siteEntities: Map[SiteType, Seq[UISiteEntity]]): UISiteLevelTrait =
    copy(siteEntities = siteEntities)

}

object UISiteLevel {

  def apply(levelType: LevelType): UISiteLevel =
    new UISiteLevel(
      levelType
      , Map(
        PLAYER -> SiteEntityBoundary.entitiesFor(levelType, PLAYER)
          .map(uiEntity)
        , LAYOUT -> SiteEntityBoundary.entitiesFor(levelType, LAYOUT)
          .map(uiEntity)
        , REGION -> SiteEntityBoundary.entitiesFor(levelType, REGION)
          .map(uiEntity)
        , PLAYLIST -> SiteEntityBoundary.entitiesFor(levelType, PLAYLIST)
          .map(uiEntity)
        , MEDIUM -> SiteEntityBoundary.entitiesFor(levelType, MEDIUM)
          .map(uiEntity)
      ))
}

case class UIFilterLevel(
                          siteEntities: Map[SiteType, Seq[UISiteEntity]] = Map()
                        )
  extends UISiteLevelTrait {
  val levelType: LevelType = FILTER

  def replaceAllEntries(siteEntities: Map[SiteType, Seq[UISiteEntity]]): UISiteLevelTrait =
    copy(siteEntities = siteEntities)

  def allSiteTypes: Seq[SiteType] = Seq(FILTER_TAG, TIMING)

  protected def filter(siteEntities: Map[SiteType, Seq[UISiteEntity]]): UISiteLevelTrait =
    copy(siteEntities = siteEntities)

}

object UIFilterLevel {
  def apply(): UIFilterLevel =
    new UIFilterLevel(
      Map(
        FILTER_TAG -> SiteEntityBoundary.entitiesFor(FILTER, FILTER_TAG)
          .map(uiEntity)
        , TIMING -> SiteEntityBoundary.entitiesFor(FILTER, TIMING)
          .map(uiEntity)
      ))
}
