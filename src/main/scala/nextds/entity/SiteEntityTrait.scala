package nextds.entity

import outwatch.dom.{VNode, tbody}

import scala.collection.mutable
import scala.language.postfixOps

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteEntityTrait {

  def siteIdent: String

  def siteType: SiteType

  def levelType: LevelType

  def label: String = s"${siteType.label} ${levelType.label}"

  lazy val ident: SiteEntityIdent = Site.nextIdent(siteIdent)

  def title: String
  def maybeTitle: Option[String]
  def descr: String
  def maybeDescr: Option[String]

  lazy val typeDefinition: String = s"$levelType-$siteType"
}

object SiteEntityTrait {
}

object SiteIdent {

  def SiteIdent(siteIdent: String, nextId: Int) =
    s"$siteIdent-$nextId"
}

case class Site(ident: String)

object Site {
  private val identOffset = 100

  def nextIdent(siteIdent: String): SiteEntityIdent = synchronized {
    val newId: Long = siteIds.get(siteIdent)
      .map(_ + 1)
      .getOrElse(identOffset)
    siteIds += (siteIdent -> newId)
    s"$siteIdent-$newId"
  }

  private val siteIds: mutable.Map[String, Long] = mutable.Map()

}

trait PlayerTrait extends SiteEntityTrait {
  val siteType = PLAYER
}

trait LayoutTrait extends SiteEntityTrait {
  val siteType = LAYOUT

  def screenRegion: ScreenRegion
  def maybeScreenRegion: Option[ScreenRegion]

}
// only for config
trait RegionTrait extends SiteEntityTrait {
  val siteType = REGION
}
trait PlaylistTrait extends SiteEntityTrait {
  val siteType = PLAYLIST
}
trait MediumTrait extends SiteEntityTrait {
  val siteType = MEDIUM
}

sealed trait SiteType {
  def name: String

  def label: String

  def logo: String
}

case object PLAYER extends SiteType {
  val name = "player"
  val label = "Player"
  val logo = "youtube-play"
}

case object LAYOUT extends SiteType {
  val name = "layout"
  val label = "Layout"
  val logo = "th-large"
}

case object REGION extends SiteType {
  val name = "region"
  val label = "Region"
  val logo = "th"
}

case object PLAYLIST extends SiteType {
  val name = "playlist"
  val label = "Playlist"
  val logo = "list-ul"
}

case object MEDIUM extends SiteType {
  val name = "medium"
  val label = "Medium"
  val logo = "medium"
}

case object TIME_FILTER extends SiteType {
  val name = "time-filter"
  val label = "Time Filter"
  val logo = "clock-o"
}

case object TAG_FILTER extends SiteType {
  val name = "time-filter"
  val label = "Tag Filter"
  val logo = "tags"
}

sealed trait LevelType {
  def name: String

  def label: String
}

case object TEMPL extends LevelType {
  val name = "templ"
  val label = "Template"
}
case object COMP extends LevelType {
  val name = "comp"
  val label = "Component"
}
case object CONF extends LevelType {
  val name = "conf"
  val label = "Configuration"
}

case object FILTER extends LevelType {
  val name = "filter"
  val label = "Filter"
}


