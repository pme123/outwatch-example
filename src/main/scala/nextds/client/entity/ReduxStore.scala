package nextds.client.entity

import nextds.client.Pages
import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary
import org.scalajs.dom.DragEvent
import outwatch.Sink
import outwatch.dom.createHandler
import rxscalajs.Observable
import rxscalajs.subscription.Subscription

import scala.language.implicitConversions

/**
  * Created by pascal.mengelt on 17.05.2017.
  */

case class ReduxStore[State, Action](initialState: State, reducer: (State, Action) => State) {
  val sink: Observable[Action] with Sink[Action] =
    createHandler[Action]()
  val source: Observable[State] =
    sink
      .scan(initialState)(reducer)
      .startWith(initialState)
      .publishReplay(1)
      .refCount

  def subscribe(f: State => Unit): Subscription = source.subscribe(f)

}

object ReduxStore {

  implicit def toSink[Action](store: ReduxStore[_, Action]): Sink[Action] = store.sink

  implicit def toSource[State](store: ReduxStore[State, _]): Observable[State] = store.source

  def apply(): ReduxStore[State, Action] = ReduxStore(State(), reducer)


  def reducer(previousState: State, action: Action): State = {
    println(s"reducer: ${action.getClass}")

    val selectedSET = selectedSETReducer(previousState, action)
    val st = State(
      siteModelReducer(previousState.siteModel, selectedSET, action)
      , selectedSET
      , draggedSETReducer(previousState.draggedSET, action)
      , linkToReducer(previousState.linkTo, action)
      , activePageReducer(previousState.activePage, action)
    )
    st
  }

  private def siteModelReducer(previousState: UISiteModel, newSelectedSET: Option[UISiteEntity], action: Action): UISiteModel = action match {
    case f: UIFilters =>
      previousState.withFilter(f)
    case Edit(uiSiteEntity) =>
      previousState.copy(selectedSET = Some(uiSiteEntity))
    case CreateFrom(_, isRegion) =>
      createFrom(previousState, newSelectedSET.get, isRegion)

    case _ => previousState
  }

  private def siteXModelReducer(previousState: UISiteModel, selectedSET: UISiteEntity, action: Action): UISiteModel = action match {
    case f: UIFilters =>
      previousState.withFilter(f)
    case Edit(uiSiteEntity) =>
      previousState.copy(selectedSET = Some(uiSiteEntity))
    case CreateFrom(siteEntityTrait, isRegion) =>
      createFrom(previousState, selectedSET, isRegion)


    case _ => previousState
  }

  private def selectedSETReducer(previousState: State, action: Action): Option[UISiteEntity] = action match {
    case Edit(uiSiteEntity) =>
      Some(uiSiteEntity)
    case CreateFrom(siteEntityTrait, isRegion) =>
      createFrom(previousState.selectedSET, siteEntityTrait, isRegion)

    case DoLinking =>
      previousState.linkTo
        .flatMap { linkTo =>
          (for {
            siteType <- linkTo.fromEntity.linkToType
            ident <- linkTo.toIdent
          } yield previousState.siteModel.entity(linkTo.fromEntity.levelType, siteType, ident))
            .orElse(linkTo.fromEntity.linkToType
              .flatMap(st => previousState.siteModel.uiSiteEntities(linkTo.fromEntity.levelType, st).uiSiteEntities.headOption)
            )
            .map(_.siteEntity.addLink(linkTo.fromEntity.siteEntity))
            .map(uiEntity)
        }.map { selected =>
        Some(selected)
      }.getOrElse(previousState.selectedSET)

    case _ => previousState.selectedSET

  }

  private def activePageReducer(previousState: Pages, action: Action): Pages = action match {
    case ChangePage(newPage) => newPage
    case _ => previousState
  }

  private def draggedSETReducer(previousState: Option[UISiteEntity], action: Action): Option[UISiteEntity] = action match {
    case DragAction(siteEntityTrait, DragEventType.start, event) =>
      event.dataTransfer.setData("text", siteEntityTrait.ident)
      event.dataTransfer.effectAllowed = "link"
      println(s"StartDrag: ${
        siteEntityTrait.ident
      } - ${
        event.`type`
      }")
      event.preventDefault()
      event.stopPropagation()
      Some(uiEntity(siteEntityTrait))
    case DragAction(siteEntityTrait, DragEventType.enter, event) =>
      event.stopPropagation()
      event.preventDefault()

      println(s"EnterDrag: ${
        siteEntityTrait.ident
      }  - ${
        event.`type`
      }")
      previousState
    case DragAction(siteEntityTrait, DragEventType.over, event) =>
      println(s"OverDrag: ${
        siteEntityTrait.ident
      }  - ${
        event.`type`
      }")
      event.preventDefault()
      previousState
    case DragAction(siteEntityTrait, DragEventType.drop, event) =>
      println(s"DataTransfer: ${
        event.dataTransfer.getData("Text")
      }")
      println(s"DropDrag: ${
        siteEntityTrait.ident
      }  - ${
        event.`type`
      }")
      event.preventDefault()
      previousState
    case DragAction(siteEntityTrait, DragEventType.end, event) =>
      println(s"EndDrag: ${
        siteEntityTrait.ident
      }  - ${
        event.`type`
      }")
      event.preventDefault()
      None
    case DragAction(siteEntityTrait, other, event) =>
      println(s"Other event: ${
        siteEntityTrait.ident
      }  - ${
        event.`type`
      }")
      event.dataTransfer.clearData("text")
      None

    case _ =>
      previousState
  }

  private def linkToReducer(previousState: Option[LinkTo], action: Action) = action match {
    case linkTo: LinkTo => Some(linkTo)
    case DoLinking => None
    case _ => previousState
  }

  private def createFrom(previousState: UISiteModel, newSET: UISiteEntity, isRegion: Boolean) = {
    previousState.replaceEntity(newSET)
  }

  private def createFrom(previousState: Option[UISiteEntity], siteEntityTrait: SiteEntityTrait, isRegion: Boolean) = {
    val newSET = SiteEntityBoundary.createFrom(siteEntityTrait, SiteEntityBoundary.siteIdent(), isRegion)
    println(s"newSET: ${
      newSET.ident
    }")
    Some(uiEntity(newSET))
  }

}

sealed trait Action

case class Edit(uiSiteEntity: UISiteEntity) extends Action

case class CreateFrom(siteEntityTrait: SiteEntityTrait, isRegion: Boolean = false) extends Action

case class LinkTo(fromEntity: UISiteEntity, toIdent: Option[String] = None) extends Action

case object DoLinking extends Action

case class DragAction(siteEntityTrait: SiteEntityTrait, event: DragEventType.Value, dragEvent: DragEvent) extends Action

case class ChangePage(newPage: Pages) extends Action

object DragEventType extends Enumeration {
  val start, enter, over, drop, end = Value
}

case class CreateFromDrag(groupFrom: String, groupTo: String, indexFrom: Int, indexTo: Option[Int] = None) extends Action

case class UIFilters(ident: Option[String] = None
                     , title: Option[String] = None
                     , sites: Option[Seq[String]] = None
                     , levels: Option[Seq[LevelType]] = None
                     , siteTypes: Option[Seq[SiteType]] = None
                     , maxEntities: Option[Int] = None) extends Action

case class State(siteModel: UISiteModel
                 , selectedSET: Option[UISiteEntity] = None
                 , draggedSET: Option[UISiteEntity] = None
                 , linkTo: Option[LinkTo] = None
                 , activePage: Pages = Pages.COMPOSER) {

}

object State {
  def apply(): State = State(UISiteModel(SiteEntityBoundary.siteModel()))
}


