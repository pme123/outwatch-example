package nextds.client.entity

import nextds.entity._

/**
  * Created by pascal.mengelt on 20.03.2017.
  */
case class UISiteModel(
                        templ: UISiteLevel = UISiteLevel(TEMPL)
                        , comp: UISiteLevel = UISiteLevel(COMP)
                        , conf: UISiteLevel = UISiteLevel(CONF)
                      ) {

  def replaceLevel(entities: UpdateEntities): UISiteModel =
    replaceLevel(level(entities.levelType).replaceEntries(entities))

  def replaceLevel(siteLevel: UISiteLevel): UISiteModel =
    siteLevel.levelType match {
      case TEMPL => copy(templ = siteLevel)
      case COMP => copy(comp = siteLevel)
      case CONF => copy(conf = siteLevel)
      case _ => this
    }

  def level(levelType: LevelType): UISiteLevel =
    levelType match {
      case TEMPL => templ
      case COMP => comp
      case CONF => conf
      case _ => templ
    }

  def allLevels: Seq[UISiteLevel] = Seq(templ, comp, conf)

  def entities(levelType: LevelType, siteType: SiteType): Seq[UISiteEntity] =
    level(levelType).entities(siteType)
}

object UISiteModel{
  def apply(): UISiteModel =
    UISiteModel(UISiteLevel(TEMPL), UISiteLevel(COMP), UISiteLevel(CONF))
}
