package nextds.client.entity

import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteConf
  extends UISiteEntity {
  def siteEntity: SiteConfTrait

  lazy val siteComp: SiteCompTrait = siteEntity.siteConf.comp

  def parameterEdit(siteEntityRefs: Seq[SiteConfTrait])(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    super.parameterEdit() ++ Seq(
      siteEntityRef(siteComp)) ++ siteEntityRefs
      .map(siteEntityRef)
  }
}

object UISiteConf {
}

case class UIPlayerConf(siteEntity: PlayerConf)
  extends UISiteConf
    with UIPlayer {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

}

case class UILayoutConf(siteEntity: LayoutConf)
  extends UISiteConf
    with UILayout {

  lazy val layoutComp: LayoutComp = siteEntity.siteConf.comp

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)


}

case class UIRegionConf(siteEntity: RegionConf)
  extends UISiteConf
    with UIRegion {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)
}

case class UIPlaylistConf(siteEntity: PlaylistConf)
  extends UISiteConf
    with UIPlaylist {
  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

}


case class UIMediumConf(siteEntity: MediumConf)
  extends UISiteConf
    with UIMedium {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

}































