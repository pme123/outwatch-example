package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State, UISiteEntity}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object ConfigViewer {

  def apply(levelType: LevelType)(implicit store: ReduxStore[State, Action]): VNode = {

    def entityListComponent(siteType: SiteType): VNode = {

      val entities =
        store.map { st =>
          val model = st.siteModel
          model.entities(levelType, siteType)
            .filter(e => model.linkedEntities.contains(e.ident))
            .take(model.maxEntries)
            .map(EntityCard.apply)
        }

      val siteEntitiesDiv =
        siteType match {
          case (REGION | PLAYLIST | MEDIUM) => bss.grid.col2
          case (_) => bss.grid.col3
        }

      div(className := siteEntitiesDiv
        , ul(id := s"config-viewer-$siteType"
          , className := css.siteEntitiesUL
          , children <-- entities)
      )

    }

    val siteLevel = {
      store.map(
        _.siteModel.level(levelType)
          .allSiteTypes
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

