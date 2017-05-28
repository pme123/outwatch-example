package nextds.client.entity

import nextds.entity._

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteComp
  extends UISiteEntity {
  def siteEntity: SiteCompTrait


}

object UISiteComp {
}

case class UIPlayerComp(siteEntity: PlayerComp)
  extends UISiteComp
    with UIPlayer {
}

case class UILayoutComp(siteEntity: LayoutComp)
  extends UISiteComp
    with UILayout {

}

case class UIPlaylistComp(siteEntity: PlaylistComp)
  extends UISiteComp
    with UIPlaylist {
}


case class UIMediumComp(siteEntity: MediumComp)
  extends UISiteComp
    with UIMedium {
}






























