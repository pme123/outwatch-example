package nextds.client

import nextds.client.entity.{State, UISiteEntity}
import org.scalajs.dom.raw.HTMLSelectElement
import outwatch.{Sink, dom}
import outwatch.dom.{createBoolHandler, createInputHandler, createStringHandler}
import outwatch.dom.helpers.InputEvent
import rxscalajs.Observable

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
package object components {
  @inline def bss = BootstrapStyles

  @inline def css = GlobalStyles

  // expose jQuery under a more familiar name
  val jQuery = JQueryStatic

  def extractSelectedOptions(ie: InputEvent): List[String] = {
    ie.target.asInstanceOf[HTMLSelectElement].options.filter(_.selected)
      .map(_.id)
      .toList
  }

  // as siteModel.withLinks should only be called if necessary (expensive)
  def checkLinks(state: State, uiEntity: UISiteEntity, doFilter: Boolean): Boolean = {
    if (state.activePage == Pages.LINKED_VIEWER) {
      if (doFilter)
        state.siteModel.filterLinks.contains(uiEntity.siteEntity)
      else
        state.siteModel.withLinks.contains(uiEntity.siteEntity)
    }
    else
      true
  }

  val filterLinksHandler: Observable[Boolean] with Sink[Boolean] = createBoolHandler(false)
  val defaultScale = "50%"
  val scaleHandler: Observable[String] with Sink[String] = createStringHandler(defaultScale)

}
