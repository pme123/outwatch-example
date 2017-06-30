package nextds.client.entity

import nextds.entity.{LevelType, _}

/**
  * Created by pascal.mengelt on 20.03.2017.
  */


case class UISiteModel(
                        siteModel: SiteModel
                        , uiSiteLevels: Map[LevelType, UISiteLevel]
                        , uiFilters: UIFilters = UIFilters()
                        , maxEntries: Int = defaultMaxEntries
                        , selectedSET: Option[UISiteEntity] = None
                      ) {

  def replaceLevel(entities: SiteEntities[_ <: SiteEntityTrait]): UISiteModel = {
    val model = siteModel.replaceLevel(entities)
    copy(siteModel = model, uiSiteLevels = UISiteModel.uiSiteLevels(model))
  }


  def uiLevel(levelType: LevelType): UISiteLevel =
    uiSiteLevels(levelType)

  def allLevels: Seq[UISiteLevel] = uiSiteLevels.values.toSeq

  def uiSiteEntities(levelType: LevelType, siteType: SiteType): UISiteEntities =
    uiLevel(levelType).uiSiteEntities(siteType)

  def entity(groupFrom: String, indexFrom: Int): UISiteEntity =
    uiLevel(LevelType.createFromGroup(groupFrom))
      .entity(SiteType.createFromGroup(groupFrom), indexFrom)

  def entity(levelType: LevelType, siteType: SiteType, ident: String): UISiteEntity =
    uiLevel(levelType)
      .entity(siteType, ident)

  def replaceEntity(set: UISiteEntity): UISiteModel =
  copy(uiSiteLevels = uiSiteLevels.updated(set.levelType, uiSiteLevels(set.levelType).replaceEntity(set)))

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
      copy(uiSiteLevels = uiSiteLevels.map {
        case (k, v) => k -> v.appendFilter(newFilter)
      }, uiFilters = newFilter)
    }
  }

  lazy val withLinks: Set[SiteEntityTrait] =
    selectedSET.map { set =>
      println(s"withLinks")
      set.siteEntity.withLinks(siteModel)
    }.getOrElse(Set())

  lazy val filterLinks: Set[SiteEntityTrait] =
    selectedSET.map { set =>
      println(s"filterLinks")
      set.siteEntity.filterLinks(withLinks)
    }.getOrElse(Set())

}

object UISiteModel {
  def apply(siteModel: SiteModel): UISiteModel =
    UISiteModel(
      siteModel
      , uiSiteLevels(siteModel)
    )

  def uiSiteLevels(siteModel: SiteModel): Map[LevelType, UISiteLevel] = {
    for ((levelType, siteLevel) <- siteModel.siteLevels)
      yield levelType -> UISiteLevel(siteLevel)
  }
}
