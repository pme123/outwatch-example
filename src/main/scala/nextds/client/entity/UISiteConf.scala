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

  lazy val siteConfRefs: Seq[UISiteConf] =
    siteEntity.siteConfRefs
      .map(uiEntity)
      .map(_.asInstanceOf[UISiteConf])

  def parameterEdit(siteEntityRefs: Seq[SiteConfTrait])(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    super.parameterEdit() ++ Seq(
      siteEntityRef(siteComp)) ++ siteEntityRefs
      .map(siteEntityRef)
  }

  override val hideMenuCreateFrom = true

  override def hideMenuCreateRegion = false


  override def withLinked(uiModel: UISiteModel): Set[SiteEntityIdent] =
    super.withLinked(uiModel) ++
      uiEntity(siteEntity.comp).withLinked(uiModel) ++
      uiModel.level(COMP).entities(siteType).map(_.ident).filter(_ == siteEntity.comp.ident) ++
      withLinkedConf(uiModel.level(CONF)) ++
      withLinkedConfUp(uiModel.level(CONF))

  // all links to the left - e.g. REGION > PLAYLIST > MEDIUM
  def withLinkedConf(siteLevel: UISiteLevelTrait): Set[SiteEntityIdent] = {
    siteConfRefs.flatMap(e => e.withLinkedConf(siteLevel)).toSet + ident
  }

  // all links to the right - e.g. REGION > LAYOUT > PLAYER
  def withLinkedConfUp(siteLevel: UISiteLevelTrait): Set[SiteEntityIdent] = {
    def inner(leftType: SiteType): Set[SiteEntityIdent] =
      (for {
        el <- siteLevel.entities(leftType).map(_.asInstanceOf[UISiteConf])
        er <- el.siteConfRefs
        if er.ident == ident
      } yield el.withLinkedConfUp(siteLevel) + el.ident)
        .flatten.toSet

    siteType match {
      case LAYOUT => inner(PLAYER)
      case REGION => inner(LAYOUT)
      case PLAYLIST => inner(REGION)
      case MEDIUM => inner(PLAYLIST)
      case _ => Set()

    }
  }


}

object UISiteConf {
}

case class UIPlayerConf(siteEntity: PlayerConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UIPlayer {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

  override def hideMenuCreateRegion = true

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

}

case class UILayoutConf(siteEntity: LayoutConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UILayout {

  lazy val layoutComp: LayoutComp = siteEntity.siteConf.comp

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

  override val menuItemLink = "link Layout to Player"
  override val linkToType: Option[SiteType] = Some(PLAYER)

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

}

case class UIRegionConf(siteEntity: RegionConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UIRegion {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

  override val menuItemLink = "link Region to Layout"
  override val linkToType: Option[SiteType] = Some(LAYOUT)

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)




}

case class UIPlaylistConf(siteEntity: PlaylistConf
                          , isFiltered: Boolean = false)
  extends UISiteConf
    with UIPlaylist {
  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

  override val menuItemLink = "link Playlist to Region"
  override val linkToType: Option[SiteType] = Some(REGION)

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)



}


case class UIMediumConf(siteEntity: MediumConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UIMedium {

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    super.parameterEdit(siteEntity.siteConfRefs)

  override val menuItemLink = "link Medium to Playlist"
  override val linkToType: Option[SiteType] = Some(PLAYLIST)

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)



}































