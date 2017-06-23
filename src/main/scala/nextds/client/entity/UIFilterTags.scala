package nextds.client.entity

import nextds.entity.{FilterTag, FilterTagConf, FilterTags}
import nextds.server.boundary.FilterTagBoundary
import outwatch.dom.VNode

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
case class UIFilterTags(filterTags: FilterTags
                        , filterTagConfs: Seq[UIFilterTagConf]) {
}

object UIFilterTags {
  def apply(filterTags: FilterTags = FilterTagBoundary.filterTags()): UIFilterTags =
    new UIFilterTags(filterTags
      , FilterTagBoundary.filterTagConfs()
        .map(UIFilterTagConf.apply))
}

case class UIFilterTagConf(filterTagConf: FilterTagConf) {
  val ident: String = filterTagConf.ident
  val condition: String = filterTagConf.condition

  val htmlCondition: VNode = condition.italic

}
