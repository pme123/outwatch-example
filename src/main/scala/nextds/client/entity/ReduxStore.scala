package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary
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
    println(s"reducer: $action - ${identity(this)}")
    action match {
      case RefreshEntities(levelType, siteType) =>
        // send the request to the server
        val entities = SiteEntityBoundary.entitiesFor(levelType, siteType)
          .map(uiEntity)
        previousState.updateEntities(UpdateEntities(levelType, siteType, entities))
      case ue: UpdateEntities =>
        previousState.updateEntities(ue)
      case Edit(siteEntityTrait) =>
        previousState.copy(selectedSET = Some(uiEntity(siteEntityTrait)))

      case CreateFrom(siteEntityTrait, isRegion) =>
        createFrom(previousState, siteEntityTrait, isRegion)

      case CreateFromDrag(groupFrom, groupTo, indexFrom, None) =>
        val uiSiteEntity = previousState.siteModel.entity(groupFrom, indexFrom)
        println(s"REDUX: $groupFrom >> $groupTo :: $indexFrom")
        createFrom(previousState
          , uiSiteEntity.siteEntity
          , SiteType.createFromGroup(groupTo).isRegion)
      case CreateFromDrag(groupFrom, groupTo, indexFrom, Some(indexTo)) =>
        val uiSiteEntity = previousState.siteModel.entity(groupFrom, indexFrom)
        println(s"REDUX: $groupFrom >> $groupTo :: $indexFrom :: $indexTo")
        println(s"draggedEntity: ${uiSiteEntity.siteEntity.ident}")
        val state = createFrom(previousState
          , uiSiteEntity.siteEntity
          , SiteType.createFromGroup(groupTo).isRegion)
        val newSET = state.selectedSET.get
        println(s"newSET: $newSET")
        val newSETs = state.siteModel.entities(newSET.levelType, newSET.siteType)
        // .filter(_.levelType == newSET.levelType)
        println("__" + newSETs.map(l => l.levelType + "-"+ l.siteType + "\n"))
        state.updateEntities(
          UpdateEntities(newSET.levelType, newSET.siteType
            , newSETs))
    }
  }

  private def createFrom(previousState: State, siteEntityTrait: SiteEntityTrait, isRegion: Boolean) = {
    val newSET = SiteEntityBoundary.createFrom(siteEntityTrait, SiteEntityBoundary.siteIdent(), isRegion)
    println(s"newSET: ${newSET.ident}")
    val newSets = previousState.siteModel.entities(newSET.levelType, newSET.siteType) :+ uiEntity(newSET)
    previousState.copy(selectedSET = Some(uiEntity(newSET)))
      .updateEntities(UpdateEntities(newSET.levelType, newSET.siteType, newSets))
  }
}

sealed trait Action

case class RefreshEntities(levelType: LevelType, siteType: SiteType) extends Action

case class UpdateEntities(levelType: LevelType, siteType: SiteType, entities: Seq[UISiteEntity]) extends Action

case class Edit(siteEntityTrait: SiteEntityTrait) extends Action

case class CreateFrom(siteEntityTrait: SiteEntityTrait, isRegion: Boolean = false) extends Action

case class CreateFromDrag(groupFrom: String, groupTo: String, indexFrom: Int, indexTo: Option[Int] = None) extends Action

case class State(siteModel: UISiteModel = UISiteModel(), selectedSET: Option[UISiteEntity] = None) {

  def updateEntities(entities: UpdateEntities): State =
    copy(siteModel = siteModel.replaceLevel(entities))
}

object State {
  def apply(): State = State(UISiteModel())
}


