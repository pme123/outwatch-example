package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.Sink
import outwatch.dom._
import rxscalajs.Observable

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object LinkedViewer {
  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {
    div(className := s"table-row ${bss.grid.row} full-height"
      , div(className := bss.grid.col9 + " full-height"
        , div(className := bss.grid.row
          , LevelComponent(TEMPL)
          , LevelComponent(COMP)
          , LevelComponent(CONF)
          , SimpleLevelComponent(FILTER, FILTER_TAG)
          , SimpleLevelComponent(TIME, TIMING)

        ))
      , div(
        UIFiltersForm()
        , EntityDetailView(bss.grid.col3)
      )
    )
  }
}
