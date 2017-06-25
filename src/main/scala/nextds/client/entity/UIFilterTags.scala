package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.FilterTagBoundary
import outwatch.Sink
import outwatch.dom._
import rxscalajs.Observable

import scala.util.{Failure, Success}

/**
  * Created by pascal.mengelt on 23.06.2017.
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

  val htmlCondition: VNode = condition.italic

  val conditionEvents: Observable[String] with Sink[String] = createStringHandler()
  private val filterCond = conditionEvents
    .map(c => FilterCond(c))

  private val condGroupClasses = filterCond
    .map {
      case Success(fc) => "has-success has-feedback"
      case Failure(fc) =>
        println(s"fc: ${fc.getMessage}")
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
      case Failure(fc) => fc.getMessage
    }


  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit() ++
      siteEntity.filterTags
        .map(ft =>
          editValue(siteType.label, div(
            tpe := "text"
            , ft.path
            , Attribute("data-toggle", "tooltip")
            , Attribute("title", ft.path)
            , disabled := true
          ))) :+
      editValue("Condition", div(className <-- condGroupClasses
        , textarea(
          className := "form-control"
          , siteEntity.condition
          , Attribute("data-toggle", "tooltip")
          , Attribute("title",
            """Use a logical condition of your Filter Tags with AND OR. Use brackets for ordering.
            Example: DE AND (FR OR EN)
            In your tag names the following characters are allowed:
             - 0-9
             - a-z
             - A-Z
             - !?_- $
          """.stripMargin)
          , inputString --> conditionEvents
        ), span(
          className <-- condSpanClasses
        ),
        child <-- condErrorMsg)
      )

  protected def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

  // all links to the level CONF
  def withLinkedUp(uiModel: UISiteModel): Set[SiteEntityIdent] = {
    uiModel.level(CONF).siteEntities.values.flatten
      .filter(_.asInstanceOf[UISiteConf].filterTagConf.exists(_.ident == ident))
      .flatMap(_.withLinkedUp(uiModel))
      .toSet + ident
  }

  // no levels below
  def withLinkedDown(uiModel: UISiteModel): Set[SiteEntityIdent] = {
    Set(ident)
  }
}
