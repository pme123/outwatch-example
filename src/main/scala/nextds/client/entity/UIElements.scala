package nextds.client.entity

import nextds.entity.SiteEntityTrait
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

    val entityInputCol: String = style(
      width(100.%%)
      , cursor.pointer
      , textDecoration := "underline"
    ).htmlClass
  }

}

trait UIElements {

  import UIElements.Style

  protected def siteEntityRef(siteEntity: SiteEntityTrait)(implicit store: ReduxStore[State, Action]): VNode = {
    editValue(siteEntity.label, div(className := Style.entityInputCol
      , tpe := "text"
      , siteEntity.ident
      , Attribute("data-toggle", "tooltip")
      , Attribute("title", siteEntity.title)
      , disabled := true
      , click(Edit(siteEntity)) --> store
    ))
  }

  protected def inputText(label: String
                          , placeholderTxt: String
                          , initVal: Option[String]
                          , tooltipTxt: Option[String] = None
                          , disabledFlag: Boolean = false): VNode = {
    editValue(label, input(className := Style.valueInputCol
      , tpe := "text"
      , placeholder := placeholderTxt
      , value := initVal.getOrElse("")
      , Attribute("data-toggle", "tooltip")
      , Attribute("title", tooltipTxt.getOrElse(""))
      , disabled := disabledFlag
    ))
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
