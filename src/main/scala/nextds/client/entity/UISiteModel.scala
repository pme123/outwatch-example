package nextds.client.entity

import nextds.entity.{LevelType, _}

/**
  * Created by pascal.mengelt on 20.03.2017.
  */


case class UISiteModel(
                        templ: UISiteLevelTrait = UISiteLevel(TEMPL)
                        , comp: UISiteLevelTrait = UISiteLevel(COMP)
                        , conf: UISiteLevelTrait = UISiteLevel(CONF)
                        , filter: UISiteLevelTrait = UIFilterLevel()
                      ) {

  def replaceLevel(entities: UpdateEntities): UISiteModel =
    replaceLevel(level(entities.levelType).replaceEntries(entities))

  def replaceLevel(siteLevel: UISiteLevelTrait): UISiteModel =
    siteLevel.levelType match {
      case TEMPL => copy(templ = siteLevel)
      case COMP => copy(comp = siteLevel)
      case CONF => copy(conf = siteLevel)
      case FILTER => copy(filter = siteLevel)
      case _ => this
    }

  def level(levelType: LevelType): UISiteLevelTrait =
    levelType match {
      case TEMPL => templ
      case COMP => comp
      case CONF => conf
      case FILTER => filter
      case _ => templ
    }

  def allLevels: Seq[UISiteLevelTrait] = Seq(templ, comp, conf, filter)

  def entities(levelType: LevelType, siteType: SiteType): Seq[UISiteEntity] =
    level(levelType).entities(siteType)

  def entity(groupFrom: String, indexFrom: Int): UISiteEntity =
    level(LevelType.createFromGroup(groupFrom))
      .entity(SiteType.createFromGroup(groupFrom), indexFrom)

}

object UISiteModel{
  def apply(): UISiteModel =
    UISiteModel(UISiteLevel(TEMPL), UISiteLevel(COMP), UISiteLevel(CONF), UIFilterLevel())
}
