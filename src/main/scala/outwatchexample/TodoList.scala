package outwatchexample

import outwatch._
import outwatch.dom._
import outwatch.util.Store

import scala.scalajs.js.JSApp

/**
  * Created by pascal.mengelt on 08.05.2017.
  */
object TodoList extends JSApp {

  sealed trait Action

  case class Add(todo: String) extends Action
  case class Delete(todo: String) extends Action
  case class UpdatedTodo(todo: String) extends Action

  case class State(text: String, todos: Seq[String])

  val initialState = State("", Seq())

  def reducer(previousState: State, action: Action) = action match {
    case Add(todo) => State("", previousState.todos :+ todo)
    case Delete(todo) => previousState.copy(todos =
      previousState.todos.filterNot(_ == todo))
    case UpdatedTodo(todo) => previousState.copy(text = todo)
  }

  val store = Store(initialState, reducer)

  def main(): Unit = {

    val listViews = store
      .map(_.todos.map(todoComponent))

    val root = div(
      textFieldComponent(),
      ul(children <-- listViews)
    )

    OutWatch.render("#app", root)
  }

  def textFieldComponent() = {

    val textValues = store.map(_.text)

    val disabledValues = textValues
      .map(_.length < 4)
      .startWith(true)

    val clearEvents = createStringHandler()

    div(
      label("Todo: "),
      input(inputString(UpdatedTodo) --> store, value <-- clearEvents),
      button(
        click(textValues.map(Add)) --> store
        , click("") --> clearEvents
        , disabled <-- disabledValues
        , "Submit"
      )
    )
  }

  def todoComponent(todo: String) = {
    li(
      span(todo),
      button(click(Delete(todo)) --> store, "Delete")
    )
  }
}
