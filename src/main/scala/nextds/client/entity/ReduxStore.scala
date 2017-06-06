package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary
import outwatch.Sink
import outwatch.dom.createHandler
import rxscalajs.Observable

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

  def subscribe(f: State => Unit) = source.subscribe(f)

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
        val newSET = SiteEntityBoundary.createFrom(siteEntityTrait, SiteEntityBoundary.siteIdent(), isRegion)
        println(s"newSET: ${newSET.ident}")
        val newSets = previousState.siteModel.entities(newSET.levelType, newSET.siteType) :+ uiEntity(newSET)
        previousState.copy(selectedSET = Some(uiEntity(newSET)))
          .updateEntities(UpdateEntities(newSET.levelType, newSET.siteType, newSets))

    }
  }

}

sealed trait Action

case class RefreshEntities(levelType: LevelType, siteType: SiteType) extends Action

case class UpdateEntities(levelType: LevelType, siteType: SiteType, entities: Seq[UISiteEntity]) extends Action

case class Edit(siteEntityTrait: SiteEntityTrait) extends Action

case class CreateFrom(siteEntityTrait: SiteEntityTrait, isRegion: Boolean = false) extends Action

case class State(siteModel: UISiteModel = UISiteModel(), selectedSET: Option[UISiteEntity] = None) {

  def updateEntities(entities: UpdateEntities): State =
    copy(siteModel = siteModel.replaceLevel(entities))
}

object State {
  def apply(): State = State(UISiteModel())
}


