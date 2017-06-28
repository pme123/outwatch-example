package nextds.client

import nextds.client.components._
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

  private val menu: VNode =
    ul(className := "nav nav-tabs"
      , menuLink(COMPOSER)
      , menuLink(LINKED_VIEWER)
      , menuLink(PLAYER_MONITOR)
      , menuLink(EXAMPLES)
    )


  val loadingSpinner: VNode = div(
    hidden <-- loadingSpinnerEvents
    , Icon.loadingIcon
  )

  val root: VNode =
    div(className := "full-height"
      , menu
      , div(className := "tab-content tab-contents"
        , pageTab(COMPOSER)
        , pageTab(LINKED_VIEWER)
        , pageTab(PLAYER_MONITOR)
        , pageTab(EXAMPLES)
      ), ModalEntitySelecter()
      , loadingSpinner
    )
}

sealed trait Pages {

  def pageId: String

  def label: String

  def active = ""

  def page(implicit store: ReduxStore[State, Action]): VNode

}

object Pages {

  case object COMPOSER extends Pages {
    val pageId: String = "composer"
    val label: String = "Composer"
    override val active = "active"

    def page(implicit store: ReduxStore[State, Action]): VNode = Composer()
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

  def menuLink(pages: Pages)(implicit store: ReduxStore[State, Action]): VNode = {
    li(className := pages.active
      , a(Attribute("data-toggle", "tab"), href := s"#${pages.pageId}"
        , pages.label
        , click(ChangePage(pages)) --> store
      ))
  }

  def pageTab(pages: Pages)(implicit store: ReduxStore[State, Action]): VNode = {
    div(id := pages.pageId
      , className := s"tab-pane fade full-height in ${pages.active}"
      , pages.page
    )
  }
}


