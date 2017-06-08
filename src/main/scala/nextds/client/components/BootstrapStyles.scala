package nextds.client.components

import nextds.client.components.Bootstrap.CommonStyle
import nextds.client.components.Bootstrap.CommonStyle._
import nextds.client.components.Icon.Icon
import outwatch.dom.Attribute

object BootstrapStyles {

def commonStyle(base: String) = (commonStyle: CommonStyle.Value) =>
 s"$base $base-$commonStyle"
  
  val buttonOpt = commonStyle("btn")

  val button = buttonOpt(default)

  val panelOpt = commonStyle("panel")


  val labelOpt = commonStyle("label")

  val label = labelOpt(default)

  val alert = commonStyle("alert")


  // wrap styles in a namespace, assign to val to prevent lazy initialization
  object modal {
    val modal = "modal"
    val fade = "fade"
    val dialog = "modal-dialog"
    val content = "modal-content"
    val header = "modal-header"
    val body = "modal-body"
    val footer = "modal-footer"
  }

  val _modal = modal

  object listGroup {
    val listGroup = "list-group"
    val item = "list-group-item"
    val itemOpt = commonStyle("list-group-item")
  }

  val _listGroup = listGroup
  val pullRight = "pull-right"
  val buttonXS = "btn-xs"
  val close = "close"

  val labelAsBadge = "label-as-badge"

  val navbar = "nav navbar-nav"

  val formGroup = "form-group"
  val formControl = "form-control"

  object grid {
    val row = "row"
    val col1 = "col-md-1"
    val col2 = "col-md-2"
    val col3 = "col-md-3"
    val col9 = "col-md-9"
    val col10 = "col-md-10"
    val col12 = "col-md-12"
  }

  object panel {
    val standard: String = commonStyle("panel")(default)
    val row = "panel-body row"
  }

  object dropdown {
    val inputGroup = "input-group-btn dropup"
    val button = "btn dropdown-toggle"
    val dataToggle = Attribute("data-toggle", "dropdown")
    def haspopup(value: Boolean) = Attribute("aria-haspopup", s"$value")
    def expanded(value: Boolean) = Attribute("aria-expanded", s"$value")
    val icon: Icon = Icon.ellipsisH
    val menu = "dropdown-menu"
  }
}
