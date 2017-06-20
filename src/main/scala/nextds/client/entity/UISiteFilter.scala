package nextds.client.entity

import nextds.entity._
import outwatch.dom.VNode

/**
  * Created by pascal.mengelt on 07.06.2017.
  */
trait UISiteFilter
  extends UISiteEntity {
  def siteEntity: SiteFilterTrait

  override val hideMenuCreateFrom = true

}

case class UITagFilter(siteEntity: TagFilter, isFiltered: Boolean = false)
  extends UISiteFilter
    with UISiteEntity {
  def filter(isFiltered:Boolean): UISiteEntity =   copy(isFiltered = isFiltered)

}
case class UITimeFilter(siteEntity: TimeFilter, isFiltered: Boolean = false)
  extends UISiteFilter
    with UISiteEntity {
  def filter(isFiltered:Boolean): UISiteEntity =   copy(isFiltered = isFiltered)

}
