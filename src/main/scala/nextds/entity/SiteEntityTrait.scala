package nextds.entity

import scala.collection.mutable
import scala.language.postfixOps

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait SiteEntityTrait {

  def siteInfo: SiteEntityInfoTrait

  def siteType: SiteType

  def levelType: LevelType

  lazy val siteIdent: SiteIdent = siteInfo.siteIdent

  def label: String = s"${siteType.label} ${levelType.label}"

  lazy val ident: SiteEntityIdent = siteInfo.ident
  lazy val title: String = siteInfo.title
  lazy val maybeTitle: Option[String] = siteInfo.maybeTitle
  lazy val descr: String = siteInfo.descr
  lazy val maybeDescr: Option[String] = siteInfo.maybeDescr
  lazy val typeDefinition: String = s"$levelType-$siteType"

  def addLink(siteEntity: SiteEntityTrait): SiteEntityTrait = this

  def withLinks(siteModel: SiteModel): Set[SiteEntityTrait] =
    withLinkedDown(siteModel) ++ withLinkedUp(siteModel)

  // all links to the level up FILTER > CONF > COMP > TEMPL
  def withLinkedUp(siteModel: SiteModel): Set[SiteEntityTrait]

  // all links to the level down TEMPL < COMP > CONF > FILTER
  def withLinkedDown(siteModel: SiteModel): Set[SiteEntityTrait]

  def filterLinks(siteEntities: Set[SiteEntityTrait]): Set[SiteEntityTrait] = siteEntities

}

object SiteEntityTrait {
}

trait SiteEntityInfoTrait {
  def siteIdent: String

  def ident: String

  def title: String

  def descr: String

  def maybeTitle: Option[String]

  def maybeDescr: Option[String]

}

case class SiteEntityInfo(siteIdent: String, ident: String, title: String, descr: String = "-")
  extends SiteEntityInfoTrait {

  def maybeTitle: Option[String] = Some(title)

  def maybeDescr: Option[String] = Some(descr)

}

object SiteEntityInfo {
  def apply(siteIdent: String, title: String): SiteEntityInfo = SiteEntityInfo(siteIdent, Site.nextIdent(siteIdent), title)
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

trait HasScreenRegion extends SiteEntityTrait {
  def screenRegion: ScreenRegion

  def maybeScreenRegion: Option[ScreenRegion]

}

trait LayoutTrait
  extends HasScreenRegion {
  val siteType = LAYOUT

}

// only for config
trait RegionTrait
  extends HasScreenRegion {
  val siteType = REGION

}

trait PlaylistTrait extends SiteEntityTrait {
  val siteType = PLAYLIST
}

trait MediumTrait extends SiteEntityTrait {
  val siteType = MEDIUM
}

trait TimeTrait extends SiteEntityTrait {
  val siteType = TIMING
  val levelType: LevelType = TIME
  override lazy val label: String = s"${siteType.label}"


  // all links to the level CONF
  def withLinkedUp(siteModel: SiteModel): Set[SiteEntityTrait] = {
    siteModel.level(CONF).siteEntities.values
      .flatMap(se => se.entities)
      .filter(_.asInstanceOf[SiteConfTrait].timingConf.exists(_.ident == ident))
      .flatMap(_.withLinkedUp(siteModel))
      .toSet + this
  }

  // no levels below
  def withLinkedDown(siteModel: SiteModel): Set[SiteEntityTrait] = {
    Set(this)
  }

}

sealed trait SiteType {
  def name: String

  def label: String

  def logo: String

  def isRegion: Boolean = false
}

object SiteType {
  def createFrom(siteStr: String): SiteType = siteStr.toLowerCase match {
    case PLAYER.name => PLAYER
    case LAYOUT.name => LAYOUT
    case REGION.name => REGION
    case PLAYLIST.name => PLAYLIST
    case MEDIUM.name => MEDIUM
    case TIMING.name => TIMING
    case FILTER_TAG.name => FILTER_TAG
  }

  def createFromGroup(groupFrom: String): SiteType =
    SiteType.createFrom(groupFrom.dropWhile(_ != '-').drop(1))

  def all = Seq(PLAYER, LAYOUT, REGION, PLAYLIST, MEDIUM, TIMING, FILTER_TAG)
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

  override def isRegion: Boolean = true

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

case object TIMING extends SiteType {
  val name = "timing"
  val label = "Timing"
  val logo = "clock-o"
}

case object FILTER_TAG extends SiteType {
  val name = "tag-filter"
  val label = "Tag Filter"
  val logo = "tags"
}

sealed trait LevelType {

  def name: String

  def label: String

  def order: Int

}

object LevelType {
  def createFrom(levelStr: String): LevelType = levelStr.toLowerCase match {
    case TEMPL.name => TEMPL
    case COMP.name => COMP
    case CONF.name => CONF
    case FILTER.name => FILTER
    case TIME.name => TIME
  }

  def createFromGroup(groupFrom: String): LevelType =
    createFrom(groupFrom.takeWhile(_ != '-'))

  def all: Seq[LevelType] = Seq(TEMPL, COMP, CONF, FILTER, TIME)
}

case object TEMPL extends LevelType {
  val name = "templ"
  val label = "Template"
  val order = 0
}

case object COMP extends LevelType {
  val name = "comp"
  val label = "Component"
  val order = 1
}

case object CONF extends LevelType {
  val name = "conf"
  val label = "Configuration"
  val order = 2
}

case object FILTER extends LevelType {
  val name = "filter"
  val label = "Filter"
  val order = 3
}

case object TIME extends LevelType {
  val name = "time"
  val label = "Time"
  val order = 4
}


