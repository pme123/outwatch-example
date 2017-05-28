package nextds.server.boundary

import nextds.entity._
import nextds.server.control.SiteTemplRepo

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
object SiteEntityBoundary {

  def entitiesFor[T <: SiteEntityTrait](levelType: LevelType, siteType: SiteType): Seq[T] =
    SiteTemplRepo.entitiesFor(levelType, siteType)
}
