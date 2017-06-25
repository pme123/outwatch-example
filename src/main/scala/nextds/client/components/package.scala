package nextds.client

import org.scalajs.dom.raw.HTMLSelectElement
import outwatch.dom.helpers.InputEvent

/**
  * Created by pascal.mengelt on 22.06.2017.
  */
package object components {
  @inline def bss = BootstrapStyles

  @inline def css = GlobalStyles

  def extractSelectedOptions(ie: InputEvent): List[String] = {
    ie.target.asInstanceOf[HTMLSelectElement].options.filter(_.selected)
      .map(_.id)
      .toList
  }

}
