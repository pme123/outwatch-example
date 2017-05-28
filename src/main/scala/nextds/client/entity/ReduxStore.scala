package nextds.client.entity

import nextds.entity._
import nextds.server.boundary.SiteEntityBoundary
import outwatch.util.Store

/**
  * Created by pascal.mengelt on 17.05.2017.
  */
object ReduxStore {

  def store() = Store(State(), reducer)

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
        previousState.copy(selectedSET = Some(siteEntityTrait))
    }
  }

}

sealed trait Action

case class RefreshEntities(levelType: LevelType, siteType: SiteType) extends Action

case class UpdateEntities(levelType: LevelType, siteType: SiteType, entities: Seq[UISiteEntity]) extends Action

case class Edit(siteEntityTrait: SiteEntityTrait) extends Action

case class State(siteModel: UISiteModel = UISiteModel(), selectedSET: Option[SiteEntityTrait] = None) {

  def updateEntities(entities: UpdateEntities): State =
    copy(siteModel = siteModel.replaceLevel(entities))
}

object State {
  def apply(): State = State(UISiteModel())
}


