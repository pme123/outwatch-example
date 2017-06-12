package nextds.client.components

import nextds.client.entity._
import nextds.entity.{LevelType, SiteEntityTrait, SiteType}
import outwatch.dom._

/**
  * Created by pascal.mengelt on 24.05.2017.
  */
object EntityDetailView {
  @inline private def bss = BootstrapStyles

  @inline private def gStyles = GlobalStyles

  object Style {

    def detailView(levelType: LevelType): String =
      Seq(
        gStyles.levelTypeStyle(levelType)
        , bss.panel.standard
      ) mkString " "

    def detailHeader(siteType: SiteType): String =
      gStyles.siteTypeStyle(siteType)


  }

  private def entityProps(uiEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    val entity = uiEntity.siteEntity
    val handler = createHandler[Seq[VNode]]()
    div(className := Style.detailView(entity.levelType)
      , table(className := "table"
        , thead(className := Style.detailHeader(entity.siteType)
          , tr(th(className := UIElements.Style.labelCol
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
    div(className := bss.grid.col3
      , hidden <-- setStream.map(_.isEmpty)
      , child <-- setStream
        .map(_.map(entityProps)
          .getOrElse(""))
    )
  }
}

object EntityCard {
  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles

  private val stylesIcon = Seq(
    css.siteEntityIcon
    , css.siteEntityElem
    , bss.grid.col1
  ) mkString " "

  private val stylesIdent = Seq(
    css.siteEntityIdent
    , css.siteEntityElem
    , bss.grid.col10
  ) mkString " "

  private val stylesMenu = Seq(
    css.siteEntityMenuIcon
    , css.siteEntityElem
    , bss.grid.col1
  ) mkString " "

  private val stylesTitle = Seq(
    css.siteEntityTitle
    , css.siteEntityElem
    , bss.grid.col12
  ) mkString " "

  def apply(uiEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    val entity = uiEntity.siteEntity

    val styles = Seq(
      bss.listGroup.item
      , bss.grid.row
      , css.siteTypeStyle(entity.siteType)
      , css.siteEntityLI
    ) mkString " "


    val selObs = store
      .map(_.selectedSET
        .exists(_.siteEntity.ident == entity.ident)
      )

    li(id := entity.ident
      , className := styles
      , selected <-- selObs
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
      val stylesButton = Seq(
        css.siteEntityMenuIcon
        , dd.button
      ) mkString " "

      div(className := dd.inputGroup
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
