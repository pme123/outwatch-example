package nextds.client.entity

import nextds.entity._
import outwatch.dom.VNode

/**
  * Created by pascal.mengelt on 07.06.2017.
  */
trait UISiteFilter
  extends UISiteEntity {
  def siteEntity: SiteFilterTrait

  override val hideMenuItem = true

}

case class UITagFilter(siteEntity: TagFilter)
  extends UISiteFilter
    with UISiteEntity {

}
case class UITimeFilter(siteEntity: TimeFilter)
  extends UISiteFilter
    with UISiteEntity {

}
