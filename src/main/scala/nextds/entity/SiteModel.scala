package nextds.entity

/**
  * Created by pascal.mengelt on 28.06.2017.
  */
case class SiteModel(siteLevels: Map[LevelType, SiteLevel]) {

  require(LevelType.all.length == siteLevels.size, s"The SiteModel's Levels must be ${LevelType.all}")

  def level(levelType: LevelType): SiteLevel =
    siteLevels(levelType)

  def levelOpt(levelType: LevelType): Option[SiteLevel] =
    siteLevels.get(levelType)

  def entities[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): Seq[SiteEntityTrait] =
    siteLevels(levelType).entities(siteType)
      .entities

  // as the levels are defined you can safely go through them
  def merge(siteModel: SiteModel) =
    SiteModel(
      for ((lt, sLevel) <- siteLevels)
        yield {
          lt ->
            siteModel.levelOpt(lt)
              .map(sLevel.merge)
              .getOrElse(sLevel)
        }
    )
}

object SiteModel {
  def empty(): SiteModel = SiteModel(LevelType.all.map(lt => lt -> SiteLevel(lt)).toMap)

  def apply(siteLevels: SiteLevel*): SiteModel = {
    empty().merge(SiteModel(siteLevels.map(sl => sl.levelType -> sl).toMap))
  }
}

case class SiteLevel(levelType: LevelType, siteEntities: Map[SiteType, SiteEntities[_ <: SiteEntityTrait]] = Map()) {

  def entities[T <: SiteEntityTrait](siteType: SiteType): SiteEntities[_ <: SiteEntityTrait] =
    siteEntities(siteType)

  def merge(siteLevel: SiteLevel): SiteLevel =
    copy(siteEntities =
      (for ((st, sEntities) <- siteLevel.siteEntities)
        yield st -> sEntities.merge(entities(st))
        ).toMap
    )

}

case class SiteEntities[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType, entities: Seq[T]) {

  def merge(siteEntities: SiteEntities[_<: SiteEntityTrait]): SiteEntities[_<: SiteEntityTrait] =
    copy(entities = siteEntities.entities ++ entities)
}

