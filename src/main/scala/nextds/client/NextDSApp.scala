package nextds.client

import nextds.client.components._
import nextds.client.entity._
import nextds.entity._
import outwatch.dom._
import rxscalajs.Observable

import scala.scalajs.js

/**
  * Created by pascal.mengelt on 17.05.2017.
  */
object NextDSApp extends js.JSApp {

  def main(): Unit = {
    val nextDS = NextDS()
    OutWatch.render("#app", nextDS.root)
    nextDS.addSorting()
  }
}

case class NextDS() {

  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles


  implicit val store: ReduxStore[State, Action] = ReduxStore()

  val listViews: Observable[Seq[VNode]] = store
    .map(
      _.siteModel.allLevels
          .map{l=>
            println(s"level ${l.levelType} - ${l.isFiltered}")
            l
          }
        .filterNot(_.isFiltered)
        .map(m => levelComponent(m.levelType))
    )

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

  def entityListComponent(levelType: LevelType, siteType: SiteType): VNode = {

    val entities =
      store.map(_.siteModel.entities(levelType, siteType)
        .filterNot(_.isFiltered)
        .map(EntityCard.apply))

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

  def addSorting() {
    //  DragDrop()

  }

  val menu =
    ul(className := "nav nav-tabs"
      , li(a(Attribute("data-toggle", "tab"), href := "#composer"
        , "Composer")
      ), li(className := "active"
        , a(Attribute("data-toggle", "tab"), href := "#playerMonitor"
          , "Player Monitor")
      ), li(a(Attribute("data-toggle", "tab"), href := "#sortExample"
        , "Sort Example")
      )
    )


  val root: VNode =
    div(className := "full-height"
      , menu
      , div(className := "tab-content tab-contents"
        , div(id := "composer"
          , className := "tab-pane fade in"
          , div(className := bss.grid.row
            , div(className := bss.grid.col9
              , div(className := bss.grid.row
                // , div(className := "col-sm-10"
                , children <-- listViews
              ))
            , div(UIFiltersForm()
              , EntityDetailView()
            )
          )
        ),
        div(id := "playerMonitor"
          , className := "tab-pane fade in full-height active"
          , PlayerMonitor()
        ),
        div(id := "sortExample"
          , className := "tab-pane fade"
          , SortExample()
        )
      ), ModalEntitySelecter()
    )
}

