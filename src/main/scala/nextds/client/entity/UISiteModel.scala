package nextds.client.entity

import nextds.entity.{LevelType, _}

/**
  * Created by pascal.mengelt on 20.03.2017.
  */


case class UISiteModel(
                        siteLevels: Map[LevelType, UISiteLevelTrait]
                        , uiFilters: UIFilters = UIFilters()
                        , maxEntries: Int = defaultMaxEntries
                        , linkedEntities: Set[SiteEntityIdent] = Set()
                      ) {

  def replaceLevel(entities: UpdateEntities): UISiteModel =
    replaceLevel(level(entities.levelType).replaceEntries(entities))

  def replaceLevel(siteLevel: UISiteLevelTrait): UISiteModel =
    UISiteModel(
      siteLevels.updated(siteLevel.levelType, siteLevel)
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

  def withFilter(f: UIFilters): UISiteModel = {
    (f.maxEnties match {
      case opt@Some(me) if opt != uiFilters.maxEnties =>
        Some(copy(maxEntries = me))
      case other => None
    }).getOrElse {
      val newFilter = f match {
        case UIFilters(opt@Some(_), _, _, _, _, _) if opt != uiFilters.ident =>
          uiFilters.copy(ident = opt)
        case UIFilters(_, opt@Some(_), _, _, _, _) if opt != uiFilters.title =>
          uiFilters.copy(title = opt)
        case UIFilters(_, _, opt@Some(_), _, _, _) if opt != uiFilters.sites =>
          uiFilters.copy(sites = opt)
        case UIFilters(_, _, _, opt@Some(_), _, _) if opt != uiFilters.levels =>
          uiFilters.copy(levels = opt)
        case UIFilters(_, _, _, _, opt@Some(_), _) if opt != uiFilters.siteTypes =>
          uiFilters.copy(siteTypes = opt)
        case UIFilters(_, _, _, _, _, opt@Some(_)) if opt != uiFilters.maxEnties =>
          uiFilters.copy(maxEnties = opt)
        case _ => uiFilters
      }
      UISiteModel(siteLevels.map {
        case (k, v) => k -> v.appendFilter(newFilter)
      }, newFilter)
    }
  }

  def withLinks(siteEntityTrait: SiteEntityTrait): _root_.nextds.client.entity.UISiteModel =
    copy(linkedEntities =
      uiEntity(siteEntityTrait).withLinked(this)
    )

}

object UISiteModel {
  def apply(): UISiteModel =
    UISiteModel(Map(
      TEMPL -> UISiteLevel(TEMPL)
      , COMP -> UISiteLevel(COMP)
      , CONF -> UISiteLevel(CONF)
      , FILTER -> UIFilterLevel())
    )
}
