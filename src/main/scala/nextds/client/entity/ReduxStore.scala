package nextds.client.entity

import nextds.client.components.Bootstrap.CommonStyle.Value
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
    println(s"reducer: $action")
    State(siteModelReducer(previousState.siteModel, action)
      , selectedSETReducer(previousState.selectedSET, action)
      , draggedSETReducer(previousState.draggedSET, action))
  }

  private def siteModelReducer(previousState: UISiteModel, action: Action): UISiteModel = action match {
    case RefreshEntities(levelType, siteType) =>
      // send the request to the server
      val entities = SiteEntityBoundary.entitiesFor(levelType, siteType)
        .map(uiEntity)
      previousState.replaceLevel(UpdateEntities(levelType, siteType, entities))
    case ue: UpdateEntities =>
      previousState.replaceLevel(ue)

    case CreateFrom(siteEntityTrait, isRegion) =>
      createFrom(previousState, siteEntityTrait, isRegion)

    case CreateFromDrag(groupFrom, groupTo, indexFrom, None) =>
      val uiSiteEntity = previousState.entity(groupFrom, indexFrom)
      println(s"REDUX: $groupFrom >> $groupTo :: $indexFrom")
      createFrom(previousState
        , uiSiteEntity.siteEntity
        , SiteType.createFromGroup(groupTo).isRegion)

    case CreateFromDrag(groupFrom, groupTo, indexFrom, Some(indexTo)) =>
      val uiSiteEntity = previousState.entity(groupFrom, indexFrom)
      println(s"REDUX: $groupFrom >> $groupTo :: $indexFrom :: $indexTo")
      println(s"draggedEntity: ${uiSiteEntity.siteEntity.ident}")
      val state = createFrom(previousState
        , uiSiteEntity.siteEntity
        , SiteType.createFromGroup(groupTo).isRegion)
      /*  val newSET = state.get
        println(s"newSET: $newSET")
        val newSETs = state.entities(newSET.levelType, newSET.siteType)
        // .filter(_.levelType == newSET.levelType)
        println("__" + newSETs.map(l => l.levelType + "-"+ l.siteType + "\n"))
        state.replaceLevel(
          UpdateEntities(newSET.levelType, newSET.siteType
            , newSETs)) */
      state
    case _ => previousState
  }

  private def selectedSETReducer(previousState: Option[UISiteEntity], action: Action): Option[UISiteEntity] = action match {
    case Edit(siteEntityTrait) =>
      Some(uiEntity(siteEntityTrait))

    case CreateFrom(siteEntityTrait, isRegion) =>
      createFrom(previousState, siteEntityTrait, isRegion)
    case _ => previousState

  }

  private def draggedSETReducer(previousState: Option[UISiteEntity], action: Action): Option[UISiteEntity] = action match {
    case DragAction(siteEntityTrait, DragEventType.start, event) =>
      event.dataTransfer.setData("text", siteEntityTrait.ident)
      event.dataTransfer.effectAllowed = "link"
      println(s"StartDrag: ${siteEntityTrait.ident} - ${event.`type`}")
      event.preventDefault();
      event.stopPropagation()
      Some(uiEntity(siteEntityTrait))
    case DragAction(siteEntityTrait, DragEventType.enter, event) =>
      event.stopPropagation();
      event.preventDefault();

      println(s"EnterDrag: ${siteEntityTrait.ident}  - ${event.`type`}")
      previousState
    case DragAction(siteEntityTrait, DragEventType.over, event) =>
      println(s"OverDrag: ${siteEntityTrait.ident}  - ${event.`type`}")
      event.preventDefault()
      previousState
    case DragAction(siteEntityTrait, DragEventType.drop, event) =>
      println(s"DataTransfer: ${event.dataTransfer.getData("Text")}")
      println(s"DropDrag: ${siteEntityTrait.ident}  - ${event.`type`}")
      event.preventDefault()
      previousState
    case DragAction(siteEntityTrait, DragEventType.end, event) =>
      println(s"EndDrag: ${siteEntityTrait.ident}  - ${event.`type`}")
      event.preventDefault()
      None
    case DragAction(siteEntityTrait, other, event) =>
      println(s"Other event: ${siteEntityTrait.ident}  - ${event.`type`}")
      event.dataTransfer.clearData("text")
      None

    case _ =>
      previousState
  }


  private def createFrom(previousState: UISiteModel, siteEntityTrait: SiteEntityTrait, isRegion: Boolean) = {
    val newSET = SiteEntityBoundary.createFrom(siteEntityTrait, SiteEntityBoundary.siteIdent(), isRegion)
    val newSets = previousState.entities(newSET.levelType, newSET.siteType) :+ uiEntity(newSET)
    previousState.replaceLevel(UpdateEntities(newSET.levelType, newSET.siteType, newSets))
  }

  private def createFrom(previousState: Option[UISiteEntity], siteEntityTrait: SiteEntityTrait, isRegion: Boolean) = {
    val newSET = SiteEntityBoundary.createFrom(siteEntityTrait, SiteEntityBoundary.siteIdent(), isRegion)
    println(s"newSET: ${newSET.ident}")
    Some(uiEntity(newSET))
  }
}

sealed trait Action

case class RefreshEntities(levelType: LevelType, siteType: SiteType) extends Action

case class UpdateEntities(levelType: LevelType, siteType: SiteType, entities: Seq[UISiteEntity]) extends Action

case class Edit(siteEntityTrait: SiteEntityTrait) extends Action

case class CreateFrom(siteEntityTrait: SiteEntityTrait, isRegion: Boolean = false) extends Action

case class DragAction(siteEntityTrait: SiteEntityTrait, event: DragEventType.Value, dragEvent: DragEvent) extends Action

object DragEventType extends Enumeration {
  val start, enter, over, drop, end = Value
}

case class CreateFromDrag(groupFrom: String, groupTo: String, indexFrom: Int, indexTo: Option[Int] = None) extends Action

case class State(siteModel: UISiteModel = UISiteModel(), selectedSET: Option[UISiteEntity] = None, draggedSET: Option[UISiteEntity] = None) {

  def updateEntities(entities: UpdateEntities): State =
    copy(siteModel = siteModel.replaceLevel(entities))
}

object State {
  def apply(): State = State(UISiteModel())
}


