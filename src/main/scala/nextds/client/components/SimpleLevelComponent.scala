package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object SimpleLevelComponent {

  def apply(levelType: LevelType, siteType: SiteType, showAll: Boolean = false)(implicit store: ReduxStore[State, Action]): VNode = {
    println(s"fromSimpleComponent: $levelType - $siteType")

    val entities =
      store
        .combineLatestWith(filterLinksHandler) { (st, doFilter) =>
          val model = st.siteModel
          val selectedIdent = st.selectedSET.map(_.ident).getOrElse("-")
          model.uiSiteEntities(levelType, siteType)
            .uiSiteEntities
            .filter(e => showAll || checkLinks(st, e, doFilter))
            .filterNot(_.isFiltered)
            .take(model.maxEntries)
            .map(e => div(className := bss.grid.col2
              , EntityCard(e, selectedIdent))
            )
        }


    div(className := css.siteLevelDiv(levelType)
      , div(className := css.siteLevelInnerDiv(levelType)
        , id := s"$levelType"
        , children <-- entities
      ))
  }

}

