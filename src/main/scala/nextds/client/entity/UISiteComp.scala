package nextds.client.entity

import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteComp
  extends UISiteEntity {
  def siteEntity: SiteCompTrait

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    customParamEdit()
  }

  def customParamEdit(customNodes: VNode*)(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    super.parameterEdit() ++
      customNodes :+ siteEntityRef(siteEntity.siteComp.templ)
  }
}

object UISiteComp {
}

case class UIPlayerComp(siteEntity: PlayerComp)
  extends UISiteComp
    with UIPlayer {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    customParamEdit(
      inputNumber("Latitude"
        , ""
        , siteEntity.maybeLocation.map(_.lat))
      , inputNumber("Longitude"
        , ""
        , siteEntity.maybeLocation.map(_.lng))
    )
  }

}

case class UILayoutComp(siteEntity: LayoutComp)
  extends UISiteComp
    with UILayout {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    customParamEdit(
        inputNumber("From Left"
          , siteEntity.screenRegion.fromLeft.toString
          , siteEntity.maybeScreenRegion.map(_.fromLeft))
        ,inputNumber("From Top"
          , siteEntity.screenRegion.fromTop.toString
          , siteEntity.maybeScreenRegion.map(_.fromLeft))
        ,inputNumber("Width"
          , siteEntity.screenRegion.width.toString
          , siteEntity.maybeScreenRegion.map(_.width))
        ,inputNumber("From Left"
          , siteEntity.screenRegion.height.toString
          , siteEntity.maybeScreenRegion.map(_.height))
      )
  }

}

case class UIPlaylistComp(siteEntity: PlaylistComp)
  extends UISiteComp
    with UIPlaylist {
}


case class UIMediumComp(siteEntity: MediumComp)
  extends UISiteComp
    with UIMedium {
}






























