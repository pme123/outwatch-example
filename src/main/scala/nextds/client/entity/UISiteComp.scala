package nextds.client.entity

import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteComp
  extends UISiteEntity {
  def siteEntity: SiteCompTrait

  lazy val templ: SiteTemplTrait = siteEntity.templ

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    customParamEdit()
  }

  def customParamEdit(customNodes: VNode*)(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    super.parameterEdit() ++
      customNodes :+ siteEntityRef(uiEntity(templ))
  }

  override val menuItemCreateFrom = s"create ${siteType.label} ${CONF.label}"

}

object UISiteComp {
}

case class UIPlayerComp(siteEntity: PlayerComp
                        , isFiltered: Boolean = false)
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

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}

case class UILayoutComp(siteEntity: LayoutComp
                        , isFiltered: Boolean = false)
  extends UISiteComp
    with UILayout {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    customParamEdit(
      inputNumber("From Left"
        , siteEntity.screenRegion.fromLeft.toString
        , siteEntity.maybeScreenRegion.map(_.fromLeft))
      , inputNumber("From Top"
        , siteEntity.screenRegion.fromTop.toString
        , siteEntity.maybeScreenRegion.map(_.fromLeft))
      , inputNumber("Width"
        , siteEntity.screenRegion.width.toString
        , siteEntity.maybeScreenRegion.map(_.width))
      , inputNumber("From Left"
        , siteEntity.screenRegion.height.toString
        , siteEntity.maybeScreenRegion.map(_.height))
    )
  }

  override val hideMenuCreateRegion = false

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}

case class UIPlaylistComp(siteEntity: PlaylistComp
                          , isFiltered: Boolean = false)
  extends UISiteComp
    with UIPlaylist {
  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}


case class UIMediumComp(siteEntity: MediumComp
                        , isFiltered: Boolean = false)
  extends UISiteComp
    with UIMedium {
  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

  override def createPreview(): VNode = {
    img(src := siteEntity.url
    , width:=siteEntity.size.width.scaled
    , height:=siteEntity.size.height.scaled)
  }
}






























