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
        , h4("View Filters")
        , label(forLabel := "identField"
          , "Ident:")
        , input(`type` := "text"
          , className := "form-control"
          , id := "identField"
          , blur ((ie: InputEvent) => UIFilters(ident = Some(ie.target.value))) --> store
        )
        , label(forLabel := "titleField"
          , "Title:")
        , input(`type` := "text"
          , className := "form-control"
          , id := "titleField"
          , blur ((ie: InputEvent) => UIFilters(title = Some(ie.target.value))) --> store
        )
        , label(forLabel := "sitesField"
          , "Sites:")
        , select(className := "form-control"
          , id := "sitesField"
          , multiple := true
          , children <-- Observable.from(Future(SiteEntityBoundary.allSites().map(s => option(id := s, s))))
          , change { (ie: InputEvent) =>
            UIFilters(sites = Some(extractSelectedOptions(ie)))
          } --> store
        ), label(forLabel := "levelTypeField"
          , "Level Types:")
        , select(className := "form-control"
          , id := "levelTypeField"
          , multiple := true
          , children <-- Observable.from(Future(LevelType.all.map(t => option(id := t.name, t.label))))
          , change { (ie: InputEvent) =>
            UIFilters(levels = Some(extractSelectedOptions(ie).map(LevelType.createFrom)))
          } --> store
        )
        , label(forLabel := "siteTypeField"
          , "Site Types:")
        , select(className := "form-control"
          , id := "siteTypeField"
          , multiple := true
          , children <-- Observable.from(Future(SiteType.all.map(t => option(id := t.name, t.label))))
          , change { (ie: InputEvent) =>
            UIFilters(siteTypes = Some(extractSelectedOptions(ie).map(SiteType.createFrom)))
          } --> store        )
        , label(forLabel := "maxEntitiesField"
          , "Display max Entities:")
        , input(`type` := "number"
          , className := "form-control"
          , id := "maxEntitiesField"
          , blur { (ie: InputEvent) =>
            val strValue = ie.target.value
            UIFilters(maxEnties = Some(if(strValue.isEmpty) defaultMaxEntries else strValue.toInt))
          }--> store
        )

      )

    )

  private def extractSelectedOptions(ie: InputEvent): List[String] = {
    ie.target.asInstanceOf[HTMLSelectElement].options.filter(_.selected)
      .map(_.id)
      .toList
  }
}
