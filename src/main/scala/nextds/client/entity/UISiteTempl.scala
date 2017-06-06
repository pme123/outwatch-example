package nextds.client.entity

import nextds.entity._
import outwatch.dom.{VNode, s, tbody}

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteTempl
  extends UISiteEntity {
  def siteEntity: SiteTemplTrait

  override val menuItemCreate = s"create ${siteType.label} ${COMP.label}"

}

object UISiteTempl {
}

case class UIPlayerTempl(siteEntity: PlayerTempl)
  extends UISiteTempl
    with UIPlayer {
}

case class UILayoutTempl(siteEntity: LayoutTempl)
  extends UISiteTempl
    with UILayout {

}

case class UIPlaylistTempl(siteEntity: PlaylistTempl)
  extends UISiteTempl
    with UIPlaylist {
}


case class UIMediumTempl(siteEntity: MediumTempl)
  extends UISiteTempl
    with UIMedium {
}






























