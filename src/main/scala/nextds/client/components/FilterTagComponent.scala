package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object FilterTagComponent {

  def apply(showAll:Boolean = false)(implicit store: ReduxStore[State, Action]): VNode = {

    val entities =
      store.map { st =>
        val model = st.siteModel
        model.filterTags.filterTagConfs
          .filterNot(_.isFiltered)
          .filter(e => showAll || model.linkedEntities.contains(e.ident))
          .take(model.maxEntries)
          .map(FilterTagCard.apply)
      }

    div(className := css.siteLevelDiv(FILTER)
      , div(className := css.siteLevelInnerDiv(FILTER)
        , id := s"$FILTER"
        , children <-- entities
      ))
  }

}

