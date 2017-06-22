package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.dom._
import rxscalajs.Observable

/**
  * Created by pascal.mengelt on 20.06.2017.
  */
object Composer {

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {

    val listViews: Observable[Seq[VNode]] = store
      .map(
        _.siteModel.allLevels
          .map(m => LevelComponent(m.levelType))
      )

    div(className := bss.grid.row + " full-height"
      , div(className := bss.grid.col9 + " full-height"
        , div(className := bss.grid.row
          // , div(className := "col-sm-10"
          , children <-- listViews
        ))
      , div(
        UIFiltersForm()
        , EntityDetailView()
      )
    )

  }
}
