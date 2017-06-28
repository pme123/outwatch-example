package nextds.server.boundary

import nextds.entity._
import nextds.server.control.FilterTagCreator

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
object FilterTagBoundary {


  def filterTags(): FilterTags = FilterTagCreator.filterTags

  def filterTagConfs(): Seq[FilterTagConf] = FilterTagCreator.filterTagConfs


}
