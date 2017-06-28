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

  implicit val store: ReduxStore[State, Action] = ReduxStore()


  def addSorting() {
    //  DragDrop()

  }

  private val menu: VNode =
    ul(className := "nav nav-tabs"
      , li(className := "active"
        , a(Attribute("data-toggle", "tab"), href := "#composer"
          , "Composer"
          , click(ChangePage(Pages.COMPOSER)) --> store
        )
      ), li(a(Attribute("data-toggle", "tab"), href := "#linkedViewer"
        , "Linked Viewer"
        , click(ChangePage(Pages.LINKED_VIEWER)) --> store
      )
      ), li(a(Attribute("data-toggle", "tab"), href := "#playerMonitor"
        , "Player Monitor"
        , click(ChangePage(Pages.PLAYER_MONITOR)) --> store
      )
      ), li(a(Attribute("data-toggle", "tab"), href := "#sortExample"
        , "Sort Example"
        , click(ChangePage(Pages.EXAMPLES)) --> store
      )
      )
    )

  val loadingSpinner: VNode = div(
    hidden <-- loadingSpinnerEvents
    , Icon.loadingIcon
  )

  val root: VNode =
    div(className := "full-height"
      , menu
      , div(className := "tab-content tab-contents"
        , div(id := "composer"
          , className := "tab-pane fade full-height in active"
          , Composer()
        ),
        div(id := "playerMonitor"
          , className := "tab-pane fade in full-height"
          , PlayerMonitor()
        ),
        div(id := "linkedViewer"
          , className := "tab-pane fade full-height"
          , LinkedViewer()
        ),
        div(id := "sortExample"
          , className := "tab-pane fade  full-height"
          , SortExample()
        )
      ), ModalEntitySelecter()
      , loadingSpinner
    )
}

sealed trait Pages {

}

object Pages {

  case object COMPOSER extends Pages

  case object LINKED_VIEWER extends Pages

  case object PLAYER_MONITOR extends Pages

  case object EXAMPLES extends Pages

}


