package nextds.client.components

import nextds.client.entity._
import nextds.entity.{LevelType, SiteEntityTrait, SiteType}
import outwatch.dom._

import scalacss.Defaults._
import scalacss.internal.mutable.StyleSheet

/**
  * Created by pascal.mengelt on 24.05.2017.
  */
object EntityDetailView {
  @inline private def bss = GlobalStyles.bootstrapStyles

  @inline private def gStyles = GlobalStyles

  object Style extends StyleSheet.Inline {

    def detailView(levelType: LevelType): String =
      gStyles.styleClassNames(
        gStyles.levelTypeStyle(levelType)
        , bss.panel.default
      )

    def detailHeader(siteType: SiteType): String =
      gStyles.siteTypeStyle(siteType).htmlClass

    val headerRow: String = style(
    ).htmlClass

  }

  private def entityProps(uiEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    val entity = uiEntity.siteEntity
    val handler = createHandler[Seq[VNode]]()
    div(className := Style.detailView(entity.levelType)
      , table(className := "table"
        , thead(className := Style.detailHeader(entity.siteType)
          , tr(className := Style.headerRow
            , th(className := UIElements.Style.labelCol
              , EntityCard.entityIcon(entity)
            )
            , th(className := UIElements.Style.valueCol
              , EntityCard.entityIdent(entity))
          ))
        , tbody(
          uiEntity.parameterEdit(): _*
        )
      )
    )
  }

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {
    val setStream = store.map(_.selectedSET)
    div(className := bss.grid.col3.htmlClass
      , hidden <-- setStream.map(_.isEmpty)
      , child <-- setStream
        .map(_.map(entityProps)
          .getOrElse(""))
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

  def apply(uiEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    val entity = uiEntity.siteEntity

    val styles = css.styleClassNames(
      css.siteTypeStyle(entity.siteType)
      , css.siteEntityLI
      , bss.listGroup.item
      , bss.grid.row)

    li(className := styles
      , entityIcon(entity)
      , entityIdent(entity)
      , entityMenu(uiEntity)
      , div(className := stylesTitle, entity.title)
    )
  }

  def entityIcon(entity: SiteEntityTrait): VNode =
    div(className := stylesIcon, css.siteTypeIcon(entity.siteType))

  def entityIdent(entity: SiteEntityTrait)(implicit store: ReduxStore[State, Action]): VNode =
    div(className := stylesIdent
      , click(Edit(entity)) --> store
      , entity.ident)

  def entityMenu(uiEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    def entityDropdown(uiEntity: UISiteEntity): VNode = {
      val dd = bss.dropdown
      val stylesButton = css.styleClassNames(
        css.siteEntityMenuIcon
        , dd.button)

      div(className := dd.inputGroup.htmlClass
        , button(tpe := "button"
          , className := stylesButton
          , dd.dataToggle
          , dd.haspopup(true)
          , dd.expanded(false)
          , dd.icon)
        , uiEntity.createMenu()
      )
    }

    div(className := stylesMenu, entityDropdown(uiEntity))
  }


}
