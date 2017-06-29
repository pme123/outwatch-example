package nextds.client.entity

import nextds.entity._

/**
  * Created by pascal.mengelt on 17.03.2017.
  */
case class UISiteLevel(
                        siteLevel: SiteLevel
                        , uiSiteEntities: Map[SiteType, UISiteEntities]
                        , isFiltered: Boolean = false) {

  def allSiteTypes: Seq[SiteType] = Seq(PLAYER, LAYOUT, REGION, PLAYLIST, MEDIUM)

  val levelType: LevelType = siteLevel.levelType

  def entitiesForSite(siteType: SiteType): UISiteEntities =
    uiSiteEntities(siteType)

  def entity(siteType: SiteType, indexFrom: Int): UISiteEntity =
    entitiesForSite(siteType).entity(indexFrom)

  def entity(siteType: SiteType, ident: String): UISiteEntity =
    entitiesForSite(siteType).entity(ident)

  def appendFilter(filters: UIFilters): UISiteLevel = {
    copy(uiSiteEntities = uiSiteEntities.map {
      case (k, v) =>
        k -> v.appendFilter(filters)
    })
  }
  def replaceEntity(set: UISiteEntity): UISiteLevel =
    copy(uiSiteEntities = uiSiteEntities.updated(set.siteType, uiSiteEntities(set.siteType).replaceEntity(set)))

}

object UISiteLevel {

  def apply(siteLevel: SiteLevel): UISiteLevel = {
    new UISiteLevel(
      siteLevel
      , for ((siteType, siteEntities) <- siteLevel.siteEntities)
        yield (siteType, UISiteEntities(siteEntities
          , siteEntities.entities.map(uiEntity)
        ))
    )
  }
}
