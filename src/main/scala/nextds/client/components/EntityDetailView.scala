package nextds.client.components

import nextds.client.entity.{Action, Edit, State, UISiteEntity}
import nextds.entity.{LevelType, SiteEntityTrait, SiteType}
import outwatch.dom._
import outwatch.util.Store

import scalacss.Defaults._
import scalacss.internal.mutable.StyleSheet

/**
  * Created by pascal.mengelt on 24.05.2017.
  */
object EntityDetailView {
  @inline private def bss = GlobalStyles.bootstrapStyles

  @inline private def gStyles = GlobalStyles

  object Style extends StyleSheet.Inline {

    import dsl._

    def detailView(levelType: LevelType): String =
      gStyles.levelTypeStyle(levelType).htmlClass

    def detailHeader(siteType: SiteType): String =
      gStyles.siteTypeStyle(siteType).htmlClass

    val headerRow: String = style(
    ).htmlClass

    val labelCol: String = style(
      width(40.%%)
    ).htmlClass

    val valueCol: String = style(
      width(60.%%)
    ).htmlClass

    val valueInputCol: String = style(
      width(100.%%)
    ).htmlClass

  }

  private def entityProps(entity: SiteEntityTrait)(implicit store: Store[State, Action]): VNode =
    div(className := Style.detailView(entity.levelType)
      , table(className := "table"
        , thead(className := Style.detailHeader(entity.siteType)
          , tr(className := Style.headerRow
            , th(className := Style.labelCol
              , EntityCard.entityIcon(entity)
            )
            , th(className := Style.valueCol
              , EntityCard.entityIdent(entity))
          ))
        , tbody(
          inputText("Title", entity.title)
          , inputTextarea("Description", entity.descr)
        ))
    )

  private def inputText(label: String, initVal: String) = {
    editValue(label, input(className := Style.valueInputCol
      , tpe := "text"
      , placeholder := initVal
      , value := initVal)
    )
  }

  private def inputTextarea(label: String, initVal: String) = {
    editValue(label, textarea(className := Style.valueInputCol
      , cols := 3
      , placeholder := initVal
      , initVal)
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

  def apply(implicit store: Store[State, Action]): VNode = {
    val setStream = store.map(_.selectedSET)
    div(className := bss.grid.col3.htmlClass
      , hidden <-- setStream.map(_.isEmpty)
      , children <-- setStream
        .map((set: Option[SiteEntityTrait]) => Seq(
          set
            .map(entityProps)
            .getOrElse("")))
    )
  }
}

object EntityCard {
  @inline private def bss = GlobalStyles.bootstrapStyles

  @inline private def css = GlobalStyles

  private val stylesIcon = css.styleClassNames(
    css.siteEntityIcon
    , css.siteEntityElem
    , bss.grid.col1)
  private val stylesIdent = css.styleClassNames(
    css.siteEntityIdent
    , css.siteEntityElem
    , bss.grid.col10)
  private val stylesMenu = css.styleClassNames(
    css.siteEntityMenuIcon
    , css.siteEntityElem
    , bss.grid.col1)
  private val stylesTitle = css.styleClassNames(
    css.siteEntityTitle
    , css.siteEntityElem
    , bss.grid.col12)

  def apply(uiEntity: UISiteEntity)(implicit store: Store[State, Action]): VNode = {
    val entity = uiEntity.siteEntity

    val styles = css.styleClassNames(
      css.siteTypeStyle(entity.siteType)
      , css.siteEntityLI
      , bss.listGroup.item
      , bss.grid.row)


    div(className := styles
      , click(Edit(entity)) --> store
      , entityIcon(entity)
      , entityIdent(entity)
      , entityMenu(entity)
      , div(className := stylesTitle, entity.title)
    )
  }

  def entityIcon(entity: SiteEntityTrait): VNode =
    div(className := stylesIcon, css.siteTypeIcon(entity.siteType))

  def entityIdent(entity: SiteEntityTrait): VNode =
    div(className := stylesIdent, entity.ident)

  def entityMenu(entity: SiteEntityTrait)(implicit store: Store[State, Action]): VNode = {
    def entityDropdown(entityTrait: SiteEntityTrait): VNode = {
      val dd = bss.dropdown
      val stylesButton = css.styleClassNames(
        css.siteEntityMenuIcon
        , dd.button)
      val stylesMenu = css.styleClassNames(
        css.siteEntityMenu
        , dd.menu)
      val stylesMenuItem = css.styleClassNames(
        css.siteEntityMenuItem)


      div(className := dd.inputGroup.htmlClass
        , button(tpe := "button"
          , className := stylesButton
          , dd.dataToggle
          , dd.haspopup(true)
          , dd.expanded(false)
          , dd.icon)
        , ul(className := stylesMenu
          , li(className := stylesMenuItem
            , click(Edit(entityTrait)) --> store
            , "edit")
          , li(className := stylesMenuItem
            , "create")
          , li(role := "separator"
            , className := "divider")
          , li(className := stylesMenuItem
            , "delete")
        ))
    }

    div(className := stylesMenu, entityDropdown(entity))
  }


}
