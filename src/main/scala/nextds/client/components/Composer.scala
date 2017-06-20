package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import nextds.entity._
import outwatch.dom._
import rxscalajs.Observable

/**
  * Created by pascal.mengelt on 20.06.2017.
  */
object Composer {
  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {

    def entityListComponent(levelType: LevelType, siteType: SiteType): VNode = {

      val entities =
        store.map(_.siteModel.entities(levelType, siteType)
          .filterNot(_.isFiltered)
          .map(EntityCard.apply)
        )

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

    def levelComponent(levelType: LevelType): VNode = {

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

    val listViews: Observable[Seq[VNode]] = store
      .map(
        _.siteModel.allLevels
          .map(m => levelComponent(m.levelType))
      )

    div(className := bss.grid.row + " full-height"
      , div(className := bss.grid.col9 + " full-height"
        , div(className := bss.grid.row
          // , div(className := "col-sm-10"
          , children <-- listViews
        ))
      , div(
        UIFiltersForm()
        , EntityDetailView()
      )
    )

  }
}
