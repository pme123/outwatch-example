package nextds.client.components

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
          model.entities(levelType, siteType)
            .filterNot(_.isFiltered)
            .filter(e => model.linkedEntities.contains(e.ident))
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
      store.map(
        _.siteModel.level(levelType)
          .allSiteTypes
          .filterNot(siteType => levelType != CONF && siteType == REGION)
          .map(siteType =>
            entityListComponent(siteType))
      )
    }

    div(className := css.siteLevelDiv(levelType)
      , div(className := css.siteLevelInnerDiv(levelType)
        , id := s"config-viewer"
        , children <-- siteLevel
      ))
  }

}

