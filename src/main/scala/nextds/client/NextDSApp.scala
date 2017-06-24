package nextds.client

import nextds.client.components._
import nextds.client.entity._
import outwatch.dom._

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

  implicit val store: ReduxStore[State, Action] = ReduxStore()


  def addSorting() {
    //  DragDrop()

  }

  val menu: VNode =
    ul(className := "nav nav-tabs"
      , li(a(Attribute("data-toggle", "tab"), href := "#composer"
        , "Composer")
      ), li(a(Attribute("data-toggle", "tab"), href := "#configurator"
        , "Configurator")
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
          , className := "tab-pane fade full-height"
          , Composer()
        ),
        div(id := "playerMonitor"
          , className := "tab-pane fade in full-height active"
          , PlayerMonitor()
        ),
        div(id := "configurator"
          , className := "tab-pane fade full-height"
          , Configurator()
        ),
        div(id := "sortExample"
          , className := "tab-pane fade  full-height"
          , SortExample()
        )
      ), ModalEntitySelecter()
    )
}

