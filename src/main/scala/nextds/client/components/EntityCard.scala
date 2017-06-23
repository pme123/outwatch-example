package nextds.client.components

import nextds.client.entity._
import nextds.entity.SiteEntityTrait
import org.scalajs.dom.DragEvent
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
object EntityCard {

  def apply(uiEntity: UISiteEntity)(implicit store: ReduxStore[State, Action]): VNode = {
    val entity = uiEntity.siteEntity

    val selObs = store
      .map(_.selectedSET
        .exists(_.siteEntity.ident == entity.ident)
      )

    li(id := entity.ident
      , className := css.entityCardLI(entity.siteType)
      , selected <-- selObs
      // , draggable := true
      , dragenter((e: DragEvent) => DragAction(entity, DragEventType.enter, e)) --> store
      // , dragover((e:DragEvent) => DragAction(entity, DragEventType.over, e)) --> store
      , drop((e: DragEvent) => DragAction(entity, DragEventType.drop, e)) --> store
      , entityIcon(entity)
      , entityIdent(entity)
      , entityMenu(uiEntity)
      , div(className := css.entityCardTitle, entity.title)
    )
  }

  val dragEvents = createDragHandler()

  dragEvents.map(e => println("drag event: " + e.`type`))

  def entityIcon(entity: SiteEntityTrait)(implicit store: ReduxStore[State, Action]): VNode =
    div(className := css.entityCardIcon
      , Attribute("draggable", "true")
      , dragstart((e: DragEvent) => DragAction(entity, DragEventType.start, e)) --> store
      , dragend((e: DragEvent) => DragAction(entity, DragEventType.end, e)) --> store
      , css.siteTypeIcon(entity.siteType))

  def entityIdent(entity: SiteEntityTrait)(implicit store: ReduxStore[State, Action]): VNode =
    div(className := css.entityCardIdent
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

    div(className := css.entityCardMenu, entityDropdown(uiEntity))
  }


}

