package nextds.client.entity

import outwatch.dom._

import scalacss.Defaults._
import scalacss.internal.mutable.StyleSheet

/**
  * Created by pascal.mengelt on 29.05.2017.
  */
object UIElements {

  object Style extends StyleSheet.Inline {

    import dsl._

    val labelCol: String = style(
      width(40.%%)
    ).htmlClass

    val valueCol: String = style(
      width(60.%%)
    ).htmlClass

    val valueInputCol: String = style(
      width(100.%%)
    ).htmlClass

    val numberInputCol: String = style(
      width(100.%%)
      , textAlign.right
    ).htmlClass

  }

}
trait UIElements {
  import UIElements.Style

  protected def inputText(label: String, sisterVal: String, initVal: Option[String]): VNode = {
    editValue(label, input(className := Style.valueInputCol
      , tpe := "text"
      , placeholder := sisterVal
      , value := initVal.getOrElse(""))
    )
  }

  protected def inputNumber(label: String, sisterVal: String, initVal: Option[AnyVal]): VNode = {
    editValue(label, input(className := Style.numberInputCol
      , tpe := "number"
      , placeholder := sisterVal
      , value := initVal.getOrElse(""))
    )
  }

  protected def inputTextarea(label: String, sisterVal: String, initVal: Option[String]): VNode = {
    val value: String = initVal.getOrElse("")
    editValue(label, textarea(className := Style.valueInputCol
      , cols := 3
      , placeholder := sisterVal
      , value)
    )
  }

  private def editValue(label: String, input: VNode) = {
    tr(
      td(className := Style.labelCol
        , label)
      , td(
        className := Style.valueCol
        , input)
    )
  }


}
