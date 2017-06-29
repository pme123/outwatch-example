package nextds.client.entity

import nextds.client.components._
import nextds.entity._
import nextds.server.boundary.FilterTagBoundary
import outwatch.Sink
import outwatch.dom._
import rxscalajs.Observable

import scala.util.{Failure, Success}

/**
  * Created by pascal.mengelt on 23.06.2017.
  *
  */
case class UIFilterTags(filterTags: FilterTags
                        , filterTagConfs: Seq[UIFilterTagConf]) {
  def appendFilter(newFilter: UIFilters): UIFilterTags =
    copy(filterTagConfs = filterTagConfs.map(_.appendFilter(newFilter).asInstanceOf[UIFilterTagConf]))
}

object UIFilterTags {
  def apply(filterTags: FilterTags = FilterTagBoundary.filterTags()): UIFilterTags =
    new UIFilterTags(filterTags
      , FilterTagBoundary.filterTagConfs()
        .map(UIFilterTagConf(_)))
}

case class UIFilterTagConf(siteEntity: FilterTagConf
                           , isFiltered: Boolean = false) extends UISiteEntity {
  val condition: String = siteEntity.condition

  val filterTags: Seq[FilterTag] = siteEntity.filterTags

  val htmlCondition: VNode = condition.italic

  val conditionEvents: Observable[String] with Sink[String] = createStringHandler()

  private def differentFilterTags(fTags: List[FilterTag]): Boolean =
    fTags.length != filterTags.length ||
      fTags.exists(ft => !filterTags.contains(ft))

  private val filterCond = conditionEvents
    .map(c => FilterCond(c)
      .flatMap(_.resolveFilterTags(FilterTagBoundary.filterTags()))
      .map { fTags =>
        if (differentFilterTags(fTags))
          println("has different FiterTags!")
        fTags
      })

  private val condGroupClasses = filterCond
    .map {
      case Success(fc) => "has-success has-feedback"
      case Failure(fc) =>
        "has-error has-feedback"
    }
  private val condSpanClasses = filterCond
    .map {
      case Success(fc) => "glyphicon glyphicon-ok form-control-feedback"
      case Failure(fc) => "glyphicon glyphicon-remove form-control-feedback"
    }
  private val condErrorMsg = filterCond
    .map {
      case Success(fc) => ""
      case Failure(fc) => p(className := "cond-error-message"
        , fc.getMessage)
    }

  private lazy val allFilterTags = FilterTagBoundary.filterTags()

  private val proposedTags = conditionEvents
    .startWith("")
    .map(_.split(" ").toSeq)
    .map(_.last)
    .map { c =>
      if (c.length > 1)
        allFilterTags.findPossibleTags(c)
      else
        Nil
    }

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    Seq(
      editValue("Condition", div(className <-- condGroupClasses
        , textarea(
          className := "form-control"
          , siteEntity.condition
          , insert --> initTooltipSink
          , Attribute("data-toggle", "tooltip")
          , Attribute("data-html", "true")
          , Attribute("title",
            """<div class="tooltip-style"><p>Use a logical condition of your Filter Tags with AND OR. Use brackets for ordering.</p>
            <p>Examples: '<b>DE AND (FR OR EN)</b>' or short '<b>DE & (FR | EN)</b>'</>
            <p>In your tag names the following characters are allowed:</p>
             <ul><li>0-9</li>
             <li>a-z</li>
             <li>A-Z</li>
             <li>!?_- $</li></ul>
          </div>""".stripMargin)
          , inputString --> conditionEvents
        ), span(
          className <-- condSpanClasses
        ),
        child <-- condErrorMsg
        , select(className := "value-input-col"
          , disabled := true
          , size <-- proposedTags.map(_.length)
          , hidden <-- proposedTags.map(_.isEmpty)
          , children <-- proposedTags.map(_.map(t =>
            option(id := t.tag
              , selected := false
              , t.path
            )))))
      ), super.inputDescription) ++
      siteEntity.filterTags
        .map(ft =>
          editValue(siteType.label, div(
            tpe := "text"
            , ft.path
            , Attribute("data-toggle", "tooltip")
            , Attribute("title", ft.path)
            , disabled := true
          )))

  protected def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

}
