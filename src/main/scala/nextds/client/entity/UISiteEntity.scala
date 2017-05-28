package nextds.client.entity

import nextds.entity._

/**
  * Created by pascal.mengelt on 28.05.2017.
  */
trait UISiteEntity {
 def siteEntity: SiteEntityTrait
  def siteType: SiteType = siteEntity.siteType
}

trait UIPlayer extends UISiteEntity {
  def siteEntity: PlayerTrait
}
trait UILayout extends UISiteEntity {
  def siteEntity: LayoutTrait
}
// only for config
trait UIRegion extends UISiteEntity {
  def siteEntity: RegionTrait
}
trait UIPlaylist extends UISiteEntity {
  def siteEntity: PlaylistTrait
}
trait UIMedium extends UISiteEntity {
  def siteEntity: MediumTrait

}
