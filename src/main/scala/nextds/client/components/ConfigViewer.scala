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

      val stylesDiv =
        siteType match {
          case (REGION | PLAYLIST | MEDIUM) => bss.grid.col2
          case (_) => bss.grid.col3
        }


      div(className := stylesDiv
        , ul(id := s"config-viewer-$siteType"
          , className := css.siteEntityUL
          , children <-- entities)
      )

    }


    val stylesDiv1 = Seq(
      css.levelTypeStyle(levelType)
      , "level-style"
      , bss.panel.standard
    ) mkString " "
    val stylesDiv2 = Seq(
      css.levelDiv
      , css.levelTypeStyle(levelType)
      , css.panelInnerDiv
      , bss.panel.row
    ) mkString " "

    val siteLevel = {
      store.map(
        _.siteModel.level(levelType)
          .allSiteTypes
          .map(siteType =>
            entityListComponent(siteType))
      )
    }

    div(className := stylesDiv1
      , div(className := stylesDiv2
        , id := s"config-viewer"
        , children <-- siteLevel
      ))
  }

}

