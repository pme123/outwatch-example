package nextds.client

import nextds.client.components._
import nextds.client.entity.bss
import nextds.client.entity._
import outwatch.dom.{Attribute, _}
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

    loadingSpinnerEvents <-- Observable.create(obs => obs.next(true))

  }
}

case class NextDS() {

  import Pages._

  implicit val store: ReduxStore[State, Action] = ReduxStore()


  def addSorting() {
    //  DragDrop()

  }

  private val filterLink: VNode = li(className := "filter-checkbox"
    , bss.tooltip.toggle
    , bss.tooltip.placement.left
    , bss.tooltip.title("Filter Links on the Link Viewers (CONF level)")
    , insert --> initTooltipSink
    , div(hidden <-- store.map(st => st.activePage != Pages.LINKED_VIEWER)
      , label(className := "checkbox-inline"
        , input(id := "filterLinks"
          , tpe := "checkbox"
          , className := "checkbox"
          , inputChecked --> filterLinksHandler
        ), "Filter Links"
      )), div(hidden <-- store.map(st => st.activePage != Pages.DETAIL_VIEWER)
      , select(className := "checkbox-inline"
        , inputString --> scaleHandler
        , option("100%")
        , option("75%")
        , option("50%", selected := true)
        , option("25%")
        , option("10%")
      ))
  )

  private val menu: VNode =
    ul(className := "main-menu nav nav-tabs"
      , COMPOSER.menuLink
      , DETAIL_VIEWER.menuLink
      , LINKED_VIEWER.menuLink
      , PLAYER_MONITOR.menuLink
      , EXAMPLES.menuLink
      , filterLink
    )

  val loadingSpinner: VNode = div(
    hidden <-- loadingSpinnerEvents
    , Icon.loadingIcon
  )

  val root: VNode =
    div(className := "full-height"
      , menu
      , div(className := "tab-content tab-contents"
        , COMPOSER.pageTab
        , DETAIL_VIEWER.pageTab
        , LINKED_VIEWER.pageTab
        , PLAYER_MONITOR.pageTab
        , EXAMPLES.pageTab
      ), ModalEntitySelecter()
      , loadingSpinner
    )
}

sealed trait Pages {

  def pageId: String

  def label: String

  def active = ""

  def page(implicit store: ReduxStore[State, Action]): VNode

  def menuLink(implicit store: ReduxStore[State, Action]): VNode = {
    li(className := active
      , a(Attribute("data-toggle", "tab"), href := s"#$pageId"
        , label
        , click(ChangePage(this)) --> store
      )
    )
  }

  def pageTab(implicit store: ReduxStore[State, Action]): VNode = {
    div(id := pageId
      , className := s"tab-pane fade full-height in $active"
      , page
    )
  }

}

object Pages {

  case object COMPOSER extends Pages {
    val pageId: String = "composer"
    val label: String = "Composer"
    override val active = "active"

    def page(implicit store: ReduxStore[State, Action]): VNode = Composer()
  }

  case object DETAIL_VIEWER extends Pages {
    val pageId: String = "detailViewer"
    val label: String = "Detail Viewer"

    def page(implicit store: ReduxStore[State, Action]): VNode = DetailViewer()
  }

  case object LINKED_VIEWER extends Pages {
    val pageId: String = "linkedViewer"
    val label: String = "Linked Viewer"

    def page(implicit store: ReduxStore[State, Action]): VNode = LinkedViewer()
  }

  case object PLAYER_MONITOR extends Pages {
    val pageId: String = "playerMonitor"
    val label: String = "Player Monitor"

    def page(implicit store: ReduxStore[State, Action]): VNode = PlayerMonitor()
  }

  case object EXAMPLES extends Pages {
    val pageId: String = "examples"
    val label: String = "Examples"

    def page(implicit store: ReduxStore[State, Action]): VNode = SortExample()
  }


}


