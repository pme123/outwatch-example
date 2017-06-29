package nextds.entity

/**
  * Created by pascal.mengelt on 28.06.2017.
  */
case class SiteModel(siteLevels: Map[LevelType, SiteLevel]) {

  require(LevelType.all.length == siteLevels.size, s"The SiteModel's Levels must be ${LevelType.all} but is ${siteLevels.keySet}")

  def level(levelType: LevelType): SiteLevel =
    siteLevels(levelType)

  def levelOpt(levelType: LevelType): Option[SiteLevel] =
    siteLevels.get(levelType)


  def entities[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): Seq[SiteEntityTrait] =
    siteLevels(levelType).entities(siteType)
      .entities

  // as the levels are defined you can safely go through them
  def merge(newSiteLevels: Map[LevelType, SiteLevel]): SiteModel = {
    SiteModel(
      for ((lt, sLevel) <- siteLevels)
        yield {
          lt ->
            newSiteLevels.get(lt)
              .map(sLevel.merge)
              .getOrElse(sLevel)
        }
    )
  }

  def replaceLevel(entities: SiteEntities[_ <: SiteEntityTrait]): SiteModel =
    SiteModel(siteLevels.updated(entities.levelType, siteLevels(entities.levelType).replaceEntities(entities)))

  def replaceEntity(set: SiteEntityTrait): SiteModel =
    SiteModel(siteLevels.updated(set.levelType, siteLevels(set.levelType).replaceEntity(set)))

}

object SiteModel {
  lazy val empty: SiteModel = SiteModel(LevelType.all.map(lt => lt -> SiteLevel(lt)).toMap)

  def apply(siteLevels: SiteLevel*): SiteModel = {
    empty.merge(siteLevels.map(sl => sl.levelType -> sl).toMap)
  }
}

case class SiteLevel(levelType: LevelType, siteEntities: Map[SiteType, SiteEntities[_ <: SiteEntityTrait]] = Map()) {

  def entities(siteType: SiteType): SiteEntities[_ <: SiteEntityTrait] =
    siteEntities(siteType)

  def merge(siteLevel: SiteLevel): SiteLevel =
    {
      copy(siteEntities =
        (for ((st, sEntities) <- siteLevel.siteEntities)
          yield st -> siteEntities.get(st).map(sEntities.merge).getOrElse(sEntities)
          ).toMap
      )
    }

  def replaceEntities(entities: SiteEntities[_ <: SiteEntityTrait]): SiteLevel =
    copy(siteEntities = siteEntities.updated(entities.siteType, entities))

  def replaceEntity(set: SiteEntityTrait): SiteLevel =
    copy(siteEntities = siteEntities.updated(set.siteType, siteEntities(set.siteType).replaceEntity(set)))

}

case class SiteEntities[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType, entities: Seq[T]) {

  def merge(siteEntities: SiteEntities[_ <: SiteEntityTrait]): SiteEntities[_ <: SiteEntityTrait] =
    copy(entities = siteEntities.entities ++ entities)

  def replaceEntity(set: SiteEntityTrait): SiteEntities[_ <: SiteEntityTrait] =
    copy(entities = entities.filter(_.ident != set.ident))

}

