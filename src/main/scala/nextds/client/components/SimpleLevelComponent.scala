package nextds.client.components

import nextds.client.Pages
import nextds.client.entity.{Action, ReduxStore, State, UIFilterTagConf, UISiteEntity}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object SimpleLevelComponent {

  def apply(levelType: LevelType, siteType: SiteType, showAll: Boolean = false)(implicit store: ReduxStore[State, Action]): VNode = {

    val entities =
      store.map { st =>
        val uiModel = st.siteModel
        val uiLevel = uiModel.uiSiteLevels(levelType)
        uiLevel.uiSiteEntities(siteType)
          .uiSiteEntities
          .filterNot(_.isFiltered)
          .filter(e => showAll || checkLinks(st, e))
          .take(uiModel.maxEntries)
          .map {
            case f: UIFilterTagConf => FilterTagCard(f)
            case uiE =>
              div(className := bss.grid.col2
                , EntityCard(uiE)
              )
          }
      }

    div(className := css.siteLevelDiv(levelType)
      , div(className := css.siteLevelInnerDiv(levelType)
        , id := s"$levelType"
        , children <-- entities
      ))
  }

}

