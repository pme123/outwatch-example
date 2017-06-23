package nextds.client.entity

import nextds.entity.{FilterTag, FilterTagConf, FilterTags}
import nextds.server.boundary.FilterTagBoundary

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
case class UIFilterTags(filterTags: FilterTags = FilterTagBoundary.filterTags()
                        , filterTagConfs: Seq[FilterTagConf] = FilterTagBoundary.filterTagConfs()) {

}
