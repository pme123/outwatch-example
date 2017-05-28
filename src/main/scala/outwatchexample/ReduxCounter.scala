package outwatchexample

import outwatch._
import outwatch.dom._
import outwatch.util.Store

import scala.scalajs.js.JSApp

/**
  * Created by pascal.mengelt on 08.05.2017.
  */
object ReduxCounter extends JSApp {

  sealed trait Action

  case object Add extends Action
  case object Subtract extends Action

  type State = Int

  def reducer(previousState: State, action: Action) = action match {
    case Add => previousState + 1
    case Subtract => previousState - 1
  }

  val store = Store(0, reducer)
  
  def main(): Unit = {
    val additions = createHandler[Int](0)
    val subtractions = createHandler[Int]()

    val operations = additions.merge(subtractions)
      .scan(0)((acc, cur) => acc + cur)

    val root = div(
      button(click(Add) --> store, "+"),
      button(click(Subtract) --> store, "-"),
      span("Count: ", child <-- store)
    )

    OutWatch.render("#app5", root)
  }
}
