package nextds.client.entity

import outwatch.dom._

/**
  * Created by pascal.mengelt on 29.05.2017.
  */


trait UIElements {

  import UIElements.Style

  protected def siteEntityRef(uiSiteEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    editValue(uiSiteEntity.label
      , div(className := css.siteTypeStyle(uiSiteEntity.siteType)
        , div(className := Style.entityInputCol
          , uiSiteEntity.ident
          , click(Edit(uiSiteEntity)) --> store)
        , small(uiSiteEntity.title)
      ), css.levelTypeStyle(uiSiteEntity.levelType))
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

  protected def editValue(label: String, input: VNode, styleClass: String = ""): VNode = UIElements.editValue(label, input, styleClass)


}

object UIElements {

  object Style {

    val labelCol: String = "label-col"
    val valueCol: String = "value-col"
    val valueInputCol: String = "value-input-col"
    val numberInputCol: String = "number-input-col"
    val entityInputCol: String = "entity-input-col"

  }

  def editValue(label: String, input: VNode, styleClass: String = ""): VNode = {
    tr(className := styleClass
      , td(className := Style.labelCol
        , label)
      , td(
        className := Style.valueCol
        , input)
    )
  }

}
