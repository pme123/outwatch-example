package nextds.server.boundary

import nextds.entity.{FilterTag, FilterTagConf, FilterTags, SiteIdent}
import nextds.server.control.FilterTagCreator

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
object FilterTagBoundary {

  def filterTags(): FilterTags = FilterTagCreator.createFilterTags

  def filterTagConfs(): Seq[FilterTagConf] = FilterTagCreator.createFilterTagConfs


}
