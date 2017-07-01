package nextds.client.components

import nextds.client.components.Bootstrap.CommonStyle
import nextds.client.components.Bootstrap.CommonStyle._
import nextds.client.components.Icon.Icon
import nextds.client.entity._
import outwatch.dom.{default => _, _}

object BootstrapStyles {

  def commonStyle(base: String): (Bootstrap.CommonStyle.Value) => String = (commonStyle: CommonStyle.Value) =>
    s"$base $base-$commonStyle"

  val buttonOpt: (Bootstrap.CommonStyle.Value) => String = commonStyle("btn")

  val button: String = buttonOpt(default)

  val panelOpt: (Bootstrap.CommonStyle.Value) => String = commonStyle("panel")


  val labelOpt: (Bootstrap.CommonStyle.Value) => String = commonStyle("label")

  val label: String = labelOpt(default)

  val alert: (Bootstrap.CommonStyle.Value) => String = commonStyle("alert")


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

  val _modal: modal.type = modal

  object listGroup {
    val listGroup = "list-group"
    val item = "list-group-item"
    val itemOpt: (Bootstrap.CommonStyle.Value) => String = commonStyle("list-group-item")
  }

  val _listGroup: listGroup.type = listGroup
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
    val col4 = "col-md-4"
    val col5 = "col-md-5"
    val col7 = "col-md-7"
    val col9 = "col-md-9"
    val col10 = "col-md-10"
    val col12 = "col-md-12"
  }

  object panel {
    val standard: String = commonStyle("panel")(default)
    val row = "panel-body row"
  }

  object dropdown {
    val id = "dropdown"
    val inputGroup = "input-group-btn"
    val button = "btn dropdown-toggle"
    val dataToggle = Attribute("data-toggle", id)
    val icon: Icon = Icon.ellipsisH
    val menu = "dropdown-menu"
  }

  object tooltip {
    def title(txt: String) = Attribute("title", txt)

    val toggle = Attribute("data-toggle", "tooltip")

    val html = Attribute("data-html", "true")

    def divWithSimple(title: String, styleClass: String): VNode =
      div(className := styleClass
        , title
        , tooltip.toggle
        , tooltip.title(title)
        , tooltip.placement.bottom
        , insert --> initTooltipSink
      )

    object placement {
      val top = Attribute("data-placement", "top")
      val bottom = Attribute("data-placement", "bottom")
      val left = Attribute("data-placement", "left")
      val right = Attribute("data-placement", "right")
      val auto = Attribute("data-placement", "auto")
    }

  }

}
