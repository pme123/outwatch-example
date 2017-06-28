package nextds.client

import nextds.client.components.{BootstrapStyles, GlobalStyles}
import nextds.entity._
import outwatch.Sink
import outwatch.dom._
import rxscalajs.Observable

import scala.language.implicitConversions

/**
  * Created by pascal.mengelt on 28.05.2017.
  */
package object entity {

  @inline def bss = BootstrapStyles

  @inline def css = GlobalStyles

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
    case e: TimingComp => UITimingComp(e)
    case e: TimingConf => UITimingConf(e)
    case other => throw new UnsupportedOperationException(s"Not supported $other")
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

  val loadingSpinnerEvents: Observable[Boolean] with Sink[Boolean] = createBoolHandler()
}
