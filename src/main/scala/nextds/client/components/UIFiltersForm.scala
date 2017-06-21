package nextds.client.components

import nextds.client.entity._
import nextds.entity.{LevelType, SiteType}
import nextds.server.boundary.SiteEntityBoundary
import org.scalajs.dom.raw.HTMLSelectElement
import outwatch.dom._
import outwatch.dom.helpers.InputEvent
import rxscalajs.Observable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by pascal.mengelt on 19.06.2017.
  */
object UIFiltersForm {
  @inline private def bss = BootstrapStyles

  def apply()(implicit store: ReduxStore[State, Action]): VNode =
    form(
      div(className := bss.grid.col3 + " form-group"
        , div(className := "filter-view"
          , table(className := "table"
            , thead(className := bss.panel.standard + " filter-view-header"
              , tr(th(className := UIElements.Style.labelCol
                , Icon.search
              )
                , th(className := UIElements.Style.valueCol
                  , "View Filters")
              ))
            , tbody(
              UIElements.editValue("Ident", input(`type` := "text"
                , className := "form-control"
                , blur((ie: InputEvent) => UIFilters(ident = Some(ie.target.value))) --> store
              ))
              , UIElements.editValue("Title", input(`type` := "text"
                , className := "form-control"
                , blur((ie: InputEvent) => UIFilters(title = Some(ie.target.value))) --> store
              ))
              , UIElements.editValue("Sites", select(className := "form-control"
                , multiple := true
                , children <-- Observable.from(Future(SiteEntityBoundary.allSites().map(s => option(id := s, s))))
                , change { (ie: InputEvent) =>
                  UIFilters(sites = Some(extractSelectedOptions(ie)))
                } --> store
              )),
              UIElements.editValue("Level Types", select(className := "form-control"
                , multiple := true
                , children <-- Observable.from(Future(LevelType.all.map(t => option(id := t.name, t.label))))
                , change { (ie: InputEvent) =>
                  UIFilters(levels = Some(extractSelectedOptions(ie).map(LevelType.createFrom)))
                } --> store
              )),
              UIElements.editValue("Site Types", select(className := "form-control"
                , multiple := true
                , children <-- Observable.from(Future(SiteType.all.map(t => option(id := t.name, t.label))))
                , change { (ie: InputEvent) =>
                  UIFilters(siteTypes = Some(extractSelectedOptions(ie).map(SiteType.createFrom)))
                } --> store)),
              UIElements.editValue("Max. Entities", input(`type` := "text"
                , className := "form-control"
                , blur { (ie: InputEvent) =>
                  val strValue = ie.target.value
                  UIFilters(maxEnties = Some(if (strValue.isEmpty) defaultMaxEntries else strValue.toInt))
                } --> store
              ))
            )
          )
        )
      )

    )

  private def extractSelectedOptions(ie: InputEvent): List[String] = {
    ie.target.asInstanceOf[HTMLSelectElement].options.filter(_.selected)
      .map(_.id)
      .toList
  }
}
