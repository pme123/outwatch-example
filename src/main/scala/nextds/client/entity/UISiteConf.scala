package nextds.client.entity

import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteConf
  extends UISiteEntity {
  def siteEntity: SiteConfTrait

}

object UISiteConf {
}

case class UIPlayerConf(siteEntity: PlayerConf)
  extends UISiteConf
    with UIPlayer {
}

case class UILayoutConf(siteEntity: LayoutConf)
  extends UISiteConf
    with UILayout {

}

case class UIRegionConf(siteEntity: RegionConf)
  extends UISiteConf
    with UIRegion {

}

case class UIPlaylistConf(siteEntity: PlaylistConf)
  extends UISiteConf
    with UIPlaylist {
}


case class UIMediumConf(siteEntity: MediumConf)
  extends UISiteConf
    with UIMedium {
}






























