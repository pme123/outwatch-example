package nextds.client.components

import org.scalajs.dom.MouseEvent
import org.scalajs.dom.raw.DragEvent
import outwatch.dom._
import rxscalajs.Observer

import scala.scalajs.js

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
object SortExample {


  val dragStartHandler = createDragHandler()
  dragStartHandler.subscribe(new Observer[DragEvent] {
    def next(de: DragEvent): Unit ={
      //de.dataTransfer.setData("text/plain", de.target.textContent)
      //  de.dataTransfer.setData("text/html", de.target.dataset.age)

      println(s"dragStart on ${de.target}")
    }

    def error(err: js.Any): Unit = println(s"Error: $err")

    def complete(): Unit = println("completed")
  })
  val dragEndHandler = createDragHandler()
  dragEndHandler.subscribe(new Observer[DragEvent] {
    def next(t: DragEvent): Unit =println(s"dragEnd on ${t.target}")

    def error(err: js.Any): Unit = println(s"Error: $err")

    def complete(): Unit = println("completed")
  })
  val dropHandler = createDragHandler()
  dropHandler.subscribe(new Observer[DragEvent] {
    def next(t: DragEvent): Unit = println(s"dropped on ${t.target}")

    def error(err: js.Any): Unit = println(s"Error: $err")

    def complete(): Unit = println("completed")
  })

  val clickHandler = createMouseHandler()
  clickHandler.subscribe(new Observer[MouseEvent] {
    def next(t: MouseEvent): Unit = println(s"clickHandler on ${t.target}")

    def error(err: js.Any): Unit = println(s"Error: $err")

    def complete(): Unit = println("completed")
  })

  def apply():VNode =
  div(ul(id := "members"
    , li(Attribute("draggable", "true"), click --> clickHandler, data.age := "38", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Brian Albers")
    , li(Attribute("draggable", "true"), data.age := "25", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Frank Salim")
    , li(Attribute("draggable", "true"), data.age := "47", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Jennifer Clark")
    , li(Attribute("draggable", "true"), data.age := "18", dragstart --> dragStartHandler, dragend --> dragEndHandler, "John Kemble")
    , li(Attribute("draggable", "true"), data.age := "20", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Lorraine Gaunce")
    , li(Attribute("draggable", "true"), data.age := "30", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Mark Wang")
    , li(Attribute("draggable", "true"), data.age := "41", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Morgan Stephen")
    , li(Attribute("draggable", "true"), data.age := "39", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Peter Lubbers")
    , li(Attribute("draggable", "true"), data.age := "33", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Vanessa Combs")
    , li(Attribute("draggable", "true"), data.age := "54", dragstart --> dragStartHandler, dragend --> dragEndHandler, "Vivian Lopez")
  )
    , div(className := "dropList"
      , fieldset(id := "racersField"
        , legend("Racers (by Age):")
        , ul(id := "racers"
          , className := "drop-zone"
          , dropzone := "copy s:text/plain s:text/html"
          , drop --> dropHandler)
      ))
    , div(className := "dropList"
      , fieldset(id := "volunteersField"
        , legend("Volunteers (by Name):")
        , ul(id := "racers"
          , className := "drop-zone"
          , dropzone := "copy s:text/plain"
          , drop --> dropHandler)
      ))
  )

}
