package nextds.client.components

import nextds.client.entity._
import nextds.entity.SiteEntityTrait
import org.scalajs.dom.DragEvent
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
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
      // , draggable := true
      , dragenter((e:DragEvent) => DragAction(entity, DragEventType.enter, e)) --> store
      // , dragover((e:DragEvent) => DragAction(entity, DragEventType.over, e)) --> store
      , drop((e:DragEvent) => DragAction(entity, DragEventType.drop, e)) --> store
      , entityIcon(entity)
      , entityIdent(entity)
      , entityMenu(uiEntity)
      , div(className := stylesTitle, entity.title)
    )
  }

  val dragEvents = createDragHandler()

  dragEvents.map(e => println("drag event: " + e.`type`))

  def entityIcon(entity: SiteEntityTrait)(implicit store: ReduxStore[State, Action]): VNode =
    div(className := stylesIcon
      , Attribute("draggable", "true")
      , dragstart ((e:DragEvent) => DragAction(entity, DragEventType.start, e)) --> store
      , dragend ((e:DragEvent) => DragAction(entity, DragEventType.end, e)) --> store
      , css.siteTypeIcon(entity.siteType))

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

