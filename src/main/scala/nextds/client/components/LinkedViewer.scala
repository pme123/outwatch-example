package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity.{COMP, CONF, TEMPL}
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object LinkedViewer {
  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {
    div(className := bss.grid.row + " full-height"
      , div(className := bss.grid.col9 + " full-height"
        , div(className := bss.grid.row
          , LinkedLevelViewer(TEMPL)
          , LinkedLevelViewer(COMP)
          , LinkedLevelViewer(CONF)
          , FilterTagComponent()

        ))
      , div(
        UIFiltersForm()
        , EntityDetailView()
      )
    )  }
}
