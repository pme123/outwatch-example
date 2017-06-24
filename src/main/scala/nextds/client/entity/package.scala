package nextds.client

import nextds.entity._
import outwatch.dom._

import scala.language.implicitConversions

/**
  * Created by pascal.mengelt on 28.05.2017.
  */
package object entity {

  val defaultMaxEntries = 7

  def uiEntity(siteEntity: SiteEntityTrait): UISiteEntity = siteEntity match {
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
    case e: FilterTagConf => UIFilterTagConf(e)
  }

  val specialWord = Seq("(", ")", "AND", "OR")

  // extending StringClass to HTML capabilities
  implicit def stringToString(s: String): HTMLString = new HTMLString(s)

  class HTMLString(val s: String) {

    lazy val italic: VNode = {
      val modifiers: Seq[VDomModifier] = s.replace("(", " ( ").s.replace(")", " ) ").trim.split(" ")
        .map {
          case str if specialWord.contains(str.trim) =>
            i(str)
          case str => b(s" $str ")
        }.toSeq
      p(
        modifiers: _*
      )
    }

  }

}
