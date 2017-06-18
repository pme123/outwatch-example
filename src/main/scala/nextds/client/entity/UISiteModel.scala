package nextds.client.entity

import nextds.entity.{LevelType, _}

/**
  * Created by pascal.mengelt on 20.03.2017.
  */


case class UISiteModel(
                      siteLevels: Map[LevelType, UISiteLevelTrait]
                      ) {

  def replaceLevel(entities: UpdateEntities): UISiteModel =
    replaceLevel(level(entities.levelType).replaceEntries(entities))

  def replaceLevel(siteLevel: UISiteLevelTrait): UISiteModel =
    UISiteModel(
      siteLevels.updated(siteLevel.levelType,siteLevel)
    )

  def level(levelType: LevelType): UISiteLevelTrait =
    siteLevels(levelType)

  def allLevels: Seq[UISiteLevelTrait] = siteLevels.values.toSeq

  def entities(levelType: LevelType, siteType: SiteType): Seq[UISiteEntity] =
    level(levelType).entities(siteType)

  def entity(groupFrom: String, indexFrom: Int): UISiteEntity =
    level(LevelType.createFromGroup(groupFrom))
      .entity(SiteType.createFromGroup(groupFrom), indexFrom)

  def entity(levelType: LevelType, siteType: SiteType, ident: String): UISiteEntity =
    level(levelType)
      .entity(siteType, ident)

  def replaceEntity(set: UISiteEntity): UISiteModel =
    UISiteModel(siteLevels.updated(set.levelType, siteLevels(set.levelType).replaceEntity(set)))

}

object UISiteModel {
  def apply(): UISiteModel =
    UISiteModel(Map(
      TEMPL->UISiteLevel(TEMPL)
      ,COMP-> UISiteLevel(COMP)
      ,CONF-> UISiteLevel(CONF)
      ,FILTER-> UIFilterLevel())
    )
}
