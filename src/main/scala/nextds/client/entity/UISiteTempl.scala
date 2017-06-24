package nextds.client.entity

import nextds.entity._

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteTempl
  extends UISiteEntity {
  def siteEntity: SiteTemplTrait

  override val menuItemCreateFrom = s"create ${siteType.label} ${COMP.label}"

  // no upper levels
  def withLinkedUp(uiModel: UISiteModel): Set[SiteEntityIdent] = {
    Set(ident)
  }

  // all links to the level COMP
  def withLinkedDown(uiModel: UISiteModel): Set[SiteEntityIdent] = {
    uiModel.level(COMP).entities(siteType)
      .filter(_.asInstanceOf[UISiteComp].siteEntity.templ.ident == ident)
      .flatMap(_.withLinkedDown(uiModel))
      .toSet + ident
  }
}

object UISiteTempl {
}

case class UIPlayerTempl(siteEntity: PlayerTempl
                         , isFiltered: Boolean = false)
  extends UISiteTempl
    with UIPlayer {
  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}

case class UILayoutTempl(siteEntity: LayoutTempl
                         , isFiltered: Boolean = false)
  extends UISiteTempl
    with UILayout {
  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}

case class UIPlaylistTempl(siteEntity: PlaylistTempl
                           , isFiltered: Boolean = false)
  extends UISiteTempl
    with UIPlaylist {
  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}


case class UIMediumTempl(siteEntity: MediumTempl
                         , isFiltered: Boolean = false)
  extends UISiteTempl
    with UIMedium {
  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}






























