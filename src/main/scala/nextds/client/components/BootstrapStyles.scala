package nextds.client.components

import japgolly.univeq.UnivEq
import nextds.client.components.Bootstrap.CommonStyle
import nextds.client.components.Bootstrap.CommonStyle._
import nextds.client.components.Icon.Icon
import outwatch.dom.Attribute

import scalacss.Defaults._
import scalacss.internal.mutable

class BootstrapStyles(implicit r: mutable.Register) extends StyleSheet.Inline()(r) {

  import dsl._

  implicit val styleUnivEq: UnivEq[CommonStyle.Value] = new UnivEq[CommonStyle.Value] {}

  val csDomain = Domain.ofValues(default, primary, success, info, warning, danger)

  val contextDomain = Domain.ofValues(success, info, warning, danger)

  def commonStyle[A: UnivEq](domain: Domain[A], base: String) = styleF(domain)(opt =>
    styleS(addClassNames(base, s"$base-$opt"))
  )

  def styleWrap(classNames: String*) = style(addClassNames(classNames: _*))

  val buttonOpt = commonStyle(csDomain, "btn")

  val button = buttonOpt(default)

  val panelOpt = commonStyle(csDomain, "panel")


  val labelOpt = commonStyle(csDomain, "label")

  val label = labelOpt(default)

  val alert = commonStyle(contextDomain, "alert")


  // wrap styles in a namespace, assign to val to prevent lazy initialization
  object modal {
    val modal = styleWrap("modal")
    val fade = styleWrap("fade")
    val dialog = styleWrap("modal-dialog")
    val content = styleWrap("modal-content")
    val header = styleWrap("modal-header")
    val body = styleWrap("modal-body")
    val footer = styleWrap("modal-footer")
  }

  val _modal = modal

  object listGroup {
    val listGroup = styleWrap("list-group")
    val item = styleWrap("list-group-item")
    val itemOpt = commonStyle(contextDomain, "list-group-item")
  }

  val _listGroup = listGroup
  val pullRight = styleWrap("pull-right")
  val buttonXS = styleWrap("btn-xs")
  val close = styleWrap("close")

  val labelAsBadge = style(addClassName("label-as-badge"), borderRadius(1.em))

  val navbar = styleWrap("nav", "navbar-nav")

  val formGroup = styleWrap("form-group")
  val formControl = styleWrap("form-control")

  object grid {
    val row: StyleA = styleWrap("row")
    val col1: StyleA = styleWrap("col-md-1")
    val col2: StyleA = styleWrap("col-md-2")
    val col3: StyleA = styleWrap("col-md-3")
    val col9: StyleA = styleWrap("col-md-9")
    val col10: StyleA = styleWrap("col-md-10")
    val col12: StyleA = styleWrap("col-md-12")
  }

  object panel {
    val default: StyleA = styleWrap("panel", "panel-default")
    val body: StyleA = styleWrap("panel-body")
    val row: StyleA = styleWrap("panel-body", "row")
  }

  object dropdown {
    val inputGroup: StyleA = styleWrap("input-group-btn", "dropup")
    val button: StyleA = styleWrap("btn", "dropdown-toggle")
    val dataToggle = Attribute("data-toggle", "dropdown")
    def haspopup(value: Boolean) = Attribute("aria-haspopup", s"$value")
    def expanded(value: Boolean) = Attribute("aria-expanded", s"$value")
    val icon: Icon = Icon.ellipsisH
    val menu: StyleA = styleWrap("dropdown-menu")
  }
}
