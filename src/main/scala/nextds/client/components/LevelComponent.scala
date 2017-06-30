package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object LevelComponent {

  def apply(levelType: LevelType, showAll: Boolean = false)(implicit store: ReduxStore[State, Action]): VNode = {

    def entityListComponent(levelType: LevelType, siteType: SiteType): VNode = {

      val entities =
        store
          .combineLatestWith(filterLinksHandler) { (st, doFilter) =>
            val model = st.siteModel
            model.uiSiteEntities(levelType, siteType)
              .uiSiteEntities
              .filter(e => showAll || checkLinks(st, e, doFilter))
              .filterNot(_.isFiltered)
              .take(model.maxEntries)
              .map(EntityCard.apply)
          }

      div(className := css.siteEntitiesDiv(levelType, siteType)
        , ul(id := s"$levelType-$siteType"
          , className := css.siteEntitiesUL
          , children <-- entities)
      )

    }

    val siteLevel = {
      store.map(
        _.siteModel.uiLevel(levelType)
          .allSiteTypes
          .filterNot(siteType => levelType != CONF && siteType == REGION)
          .map(siteType =>
            entityListComponent(levelType, siteType))
      )
    }

    div(className := css.siteLevelDiv(levelType)
      , div(className := css.siteLevelInnerDiv(levelType)
        , id := s"$levelType"
        , children <-- siteLevel
      ))
  }

}

