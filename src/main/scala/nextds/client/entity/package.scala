package nextds.client

import nextds.entity._

/**
  * Created by pascal.mengelt on 28.05.2017.
  */
package object entity {

  def uiEntity(siteEntity: SiteEntityTrait) = siteEntity match {
    case e: PlayerTempl => UIPlayerTempl(e)
    case e: LayoutTempl => UILayoutTempl(e)
    case e: PlaylistTempl => UIPlaylistTempl(e)
    case e: MediumTempl => UIMediumTempl(e)
    case e: PlayerComp => UIPlayerComp(e)
    case e: LayoutComp => UILayoutComp(e)
    case e: PlaylistComp => UIPlaylistComp(e)
    case e: MediumComp => UIMediumComp(e)
    case e: PlayerConf => UIPlayerConf(e)
    case e: LayoutConf => UILayoutConf(e)
    case e: RegionConf => UIRegionConf(e)
    case e: PlaylistConf => UIPlaylistConf(e)
    case e: MediumConf => UIMediumConf(e)
    case e: TagFilter => UITagFilter(e)
    case e: TimeFilter => UITimeFilter(e)
  }
}
