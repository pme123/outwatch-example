package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary
import outwatch.Sink
import outwatch.util.Store
import rxscalajs.Observable

import scala.language.implicitConversions

/**
  * Created by pascal.mengelt on 17.05.2017.
  */

case class ReduxStore[State, Action](wrapped: Store[State, Action])

object ReduxStore {

  implicit def toSink[Action](store: ReduxStore[_, Action]): Sink[Action] = store.wrapped.sink
  implicit def toSource[State](store: ReduxStore[State, _]): Observable[State] = store.wrapped.source.share

  def apply(): ReduxStore[State, Action] = ReduxStore(Store(State(), reducer))

  def reducer(previousState: State, action: Action): State = {
    println(s"reducer: $action")
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
    }
  }

}

sealed trait Action

case class RefreshEntities(levelType: LevelType, siteType: SiteType) extends Action

case class UpdateEntities(levelType: LevelType, siteType: SiteType, entities: Seq[UISiteEntity]) extends Action

case class Edit(siteEntityTrait: SiteEntityTrait) extends Action

case class State(siteModel: UISiteModel = UISiteModel(), selectedSET: Option[UISiteEntity] = None) {

  def updateEntities(entities: UpdateEntities): State =
    copy(siteModel = siteModel.replaceLevel(entities))
}

object State {
  def apply(): State = State(UISiteModel())
}


