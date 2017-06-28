package nextds.client.entity

import nextds.entity.{LevelType, _}

/**
  * Created by pascal.mengelt on 20.03.2017.
  */


case class UISiteModel(
                        siteLevels: Map[LevelType, UISiteLevelTrait]
                        , filterTags: UIFilterTags = UIFilterTags()
                        , uiTimingConfs: UITimingConfs = UITimingConfs()
                        , uiFilters: UIFilters = UIFilters()
                        , maxEntries: Int = defaultMaxEntries
                        , selectedSET: Option[UISiteEntity] = None
                      ) {

  def replaceLevel(entities: UpdateEntities): UISiteModel =
    replaceLevel(level(entities.levelType).replaceEntries(entities))

  def replaceLevel(siteLevel: UISiteLevelTrait): UISiteModel =
    copy(
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
    copy(siteLevels.updated(set.levelType, siteLevels(set.levelType).replaceEntity(set)))

  def withFilter(f: UIFilters): UISiteModel = {
    (f.maxEntities match {
      case opt@Some(me) if opt != uiFilters.maxEntities =>
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
        case UIFilters(_, _, _, _, _, opt@Some(_)) if opt != uiFilters.maxEntities =>
          uiFilters.copy(maxEntities = opt)
        case _ => uiFilters
      }
      copy(siteLevels = siteLevels.map {
        case (k, v) => k -> v.appendFilter(newFilter)
      }, uiFilters = newFilter, filterTags = filterTags.appendFilter(newFilter)
        , uiTimingConfs = uiTimingConfs.appendFilter(newFilter))
    }
  }

  lazy val withLinks: Set[SiteEntityTrait] =
    selectedSET.map { set =>
      println(s"withLinks: $set")
      set.siteEntity.filterLinks(set.withLinked(this))
    }.getOrElse(Set())


  def simpleLevel(siteType: SiteType): Seq[UISiteEntity] = siteType match {
    case FILTER_TAG => filterTags.filterTagConfs
    case TIMING => uiTimingConfs.timingConfs
    case other => throw new UnsupportedOperationException(s"No support simple level for: $siteType")
  }

}

object UISiteModel {
  def apply(): UISiteModel =
    UISiteModel(Map(
      TEMPL -> UISiteLevel(TEMPL)
      , COMP -> UISiteLevel(COMP)
      , CONF -> UISiteLevel(CONF)
    ))
}
