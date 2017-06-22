package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
object LevelComponent {

  def apply(levelType: LevelType)(implicit store: ReduxStore[State, Action]): VNode = {

    def entityListComponent(levelType: LevelType, siteType: SiteType): VNode = {

      val entities =
        store.map {st =>
          val model = st.siteModel
          model.entities(levelType, siteType)
            .filterNot(_.isFiltered)
            .take(model.maxEntries)
            .map(EntityCard.apply)
        }

      val stylesDiv =
        (levelType, siteType) match {
          case (CONF, REGION | PLAYLIST | MEDIUM) => bss.grid.col2
          case (_, _) => bss.grid.col3
        }


      div(className := stylesDiv
        , ul(id := s"$levelType-$siteType"
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
          .filterNot(siteType => levelType != CONF && siteType == REGION)
          .map(siteType =>
            entityListComponent(levelType, siteType))
      )
    }

    div(className := stylesDiv1
      , div(className := stylesDiv2
        , id := s"$levelType"
        , children <-- siteLevel
      ))
  }

}

