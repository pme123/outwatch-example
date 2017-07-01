package nextds.client.components

import nextds.client.Pages
import nextds.client.entity.{Action, ReduxStore, State}
import outwatch.dom._

/**
  * Created by pascal.mengelt on 20.06.2017.
  */
object DetailViewer {


  def apply()(implicit store: ReduxStore[State, Action]): VNode = {

    div(className := bss.grid.row + " detail-viewer full-height"
      , EntityDetailView(bss.grid.col5)
      , child <-- store.map { st =>
        if (st.activePage == Pages.DETAIL_VIEWER) {
          val selectedEntity = st.selectedSET
          div(className := bss.grid.col7 + " full-height"
            , div(className:="detail-preview"
              , selectedEntity.map(_.createPreview()).getOrElse("")
          ))
        }
      }
    )

  }
}
