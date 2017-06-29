package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.TimingBoundary
import outwatch.dom._

/**
  * Created by pascal.mengelt on 23.06.2017.
  *
  */
case class UITimingConfs(timingConfs: Seq[UITimingConf]) {

  def appendFilter(newFilter: UIFilters): UITimingConfs =
    copy(timingConfs = timingConfs.map(_.appendFilter(newFilter).asInstanceOf[UITimingConf]))
}

object UITimingConfs {
  def apply(): UITimingConfs =
    new UITimingConfs(TimingBoundary.timingConfs()
      .map(UITimingConf(_)))
}

case class UITimingComp(siteEntity: TimingComp
                        , isFiltered: Boolean = false) extends UISiteEntity {
  protected def filter(isFiltered: Boolean): UISiteEntity = ???

}
case class UITimingConf(siteEntity: TimingConf
                        , isFiltered: Boolean = false) extends UISiteEntity {

  private val timingComps = siteEntity.timingComps.map(tc => uiEntity(tc))


  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit() ++
      timingComps.map(siteEntityRef)

  protected def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

}
