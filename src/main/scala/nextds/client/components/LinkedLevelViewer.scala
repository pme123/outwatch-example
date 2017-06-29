package nextds.client.components

import nextds.client.Pages
import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object LinkedLevelViewer {

  def apply(levelType: LevelType)(implicit store: ReduxStore[State, Action]): VNode = {

    def entityListComponent(siteType: SiteType): VNode = {

      val entities =
        store.map { st =>
          val model = st.siteModel
          model.uiSiteEntities(levelType, siteType)
            .uiSiteEntities
            .filterNot(_.isFiltered)
            .filter(e => model.withLinks.contains(e.siteEntity))
            .take(model.maxEntries)
            .map(EntityCard.apply)
        }

      div(className := css.siteEntitiesDiv(levelType, siteType)
        , ul(id := s"config-viewer-$siteType"
          , className := css.siteEntitiesUL
          , children <-- entities)
      )

    }

    val siteLevel = {
      store.map { st =>
        if (st.activePage == Pages.LINKED_VIEWER) {
          st.siteModel.uiLevel(levelType)
            .allSiteTypes
            .filterNot(siteType => levelType != CONF && siteType == REGION)
            .map(siteType =>
              entityListComponent(siteType))
        } else {
          Seq(div("other page"))
        }
      }
    }

    div(className := css.siteLevelDiv(levelType)
      , div(className := css.siteLevelInnerDiv(levelType)
        , id := s"config-viewer"
        , children <-- siteLevel
      ))
  }

}

