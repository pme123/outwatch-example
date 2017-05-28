package outwatchexample

import outwatch.Sink
import outwatch.dom._
import rxscalajs.Observable

import scala.scalajs.js.JSApp

object Outwatchexample extends JSApp {

  def main(): Unit = {
    import Components._
    var ignoreElements = false

    val stopHandler = createHandler[String]()
      .map(_ => printWatch(0, 0, 0, 0))

    def seconds = Observable.interval(100)
      .filter(_ => !ignoreElements)
      .map { totCenSec =>
        val cenSec = totCenSec % 10
        val sec = (totCenSec / 10) % 60
        val min = (totCenSec / (10 * 60)) % 60
        val hou = (totCenSec / (10 * 60 * 60)) % 60
        printWatch(cenSec, sec, min, hou)
      }.startWith(0)

    def r = div(child <-- seconds)

    var counter = 0

    def setWatch() = {
      counter += 1
      val root = div(headerComponent("First Component"
        , h2(s"$counter. Antãos Stoppuhr: ", child <-- stopHandler)
        , false)
        , r)

      OutWatch.render("#app", root)
    }

    setWatch()
    val toggleEvents = createBoolHandler()

    toggleEvents.map { b =>
      println(s"Checkk hi: $b")
      if (b)
        ignoreElements = true
      b
    }


    val root2 = div(
      label("Start Stop"),
      input(tpe := "checkbox", inputChecked --> toggleEvents)
      , h2(hidden <-- toggleEvents, "Visible!")
    )

    OutWatch.render("#app2", root2)

    val list = List("What", "Is", "Up?").map(s => li(s))
    val lists = Observable.just(list)
    val ulList = ul(children <-- lists)

    val personName = createStringHandler("")

    val root3 = div(
      personComponent("Person", personName)
      , h3("Hello, ", child <-- personName)
      , ulList
    )


    OutWatch.render("#app3", root3)

    val sliderEvents = createNumberHandler()

    val imageLists: Observable[List[VNode]] = sliderEvents
      .map(_ / 10.0)
      .map(n => List.fill(n.asInstanceOf[Int])(span(s">")))

    OutWatch.render("#app4", sliderComponent(sliderEvents, imageLists))

    val additions = createHandler[Int](0)
    val subtractions = createHandler[Int]()

    val operations = additions.merge(subtractions)
      .scan(0)((acc, cur) => acc + cur)

    val root5 = div(
      button(click(1) --> additions, "+"),
      button(click(-1) --> subtractions, "-"),
      span("Count: ", child <-- operations)
    )

    OutWatch.render("#app5", root5)

  }

  private def printWatch(cenSec: Int, sec: Int, min: Int, hou: Int) = {
    f"$hou%02d:$min%02d.$sec%02d.$cenSec"
  }

  object Components {

    def headerComponent(title: String, secondHeader: VNode, hideThird: Boolean) = {
      div(
        h1(title)
        , secondHeader
        , h2(hidden := hideThird, "This is the second title")
      )
    }

    def sliderComponent(sliderEvents: Observable[Double] with Sink[Double]
                        , imageLists: Observable[List[VNode]]) = {
      div(input(
        tpe := "range",
        inputNumber --> sliderEvents,
        value := 0
      ),
        div(children <-- imageLists)
      )
    }

    def inputComponent(labelText: String
                       , textValues: Sink[String]
                       , clearEvents: Observable[String]) = {
      div(
        label(labelText),
        input(inputString --> textValues
          , value <-- clearEvents)
      )
    }

    def personComponent(labelText: String, texts: Sink[String]) = {
      val firstNames = createStringHandler("")
      val lastNames = createStringHandler("")

      val fullNames = firstNames
        .combineLatestWith(lastNames)((first, last) => s"$first $last")

      val disableEvents = fullNames.map(_.length < 5)
      val clearEvents = createStringHandler()

      ul(
        div(label(labelText)),
        inputComponent("First Name", firstNames, clearEvents),
        inputComponent("Last Name", lastNames, clearEvents),
        div(button(click(fullNames) --> texts
          , click("") --> clearEvents
          , disabled <-- disableEvents
          , "Submit"))
      )
    }
  }

}
