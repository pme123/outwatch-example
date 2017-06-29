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
      .map { st =>
        val model = st.siteModel
        model.allLevels
            .sortBy(_.levelType.order)
          .map { m =>
            m.levelType match {
              case TIME => SimpleLevelComponent(m.levelType, TIMING, showAll = true)
              case FILTER => SimpleLevelComponent(m.levelType, FILTER_TAG, showAll = true)
              case _ => LevelComponent(m.levelType, showAll = true)
            }
          }
      }

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
