package nextds.client.entity

import nextds.client.components.scaleHandler
import nextds.entity._
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.Element
import org.scalajs.dom.{CanvasRenderingContext2D, window}
import outwatch.Sink
import outwatch.dom.{update, _}

/**
  * Created by pascal.mengelt on 15.03.2017.
  */
trait UISiteConf
  extends UISiteEntity {
  def siteEntity: SiteConfTrait

  lazy val siteComp: SiteCompTrait = siteEntity.siteInfo.comp

  lazy val siteConfRefs: Seq[UISiteConf] =
    siteEntity.siteConfRefs
      .map(uiEntity)
      .map(_.asInstanceOf[UISiteConf])

  lazy val filterTagConf: Option[UIFilterTagConf] =
    siteEntity.filterTagConf
      .map(uiEntity(_).asInstanceOf[UIFilterTagConf])

  lazy val timingConf: Option[UITimingConf] =
    siteEntity.timingConf
      .map(uiEntity(_).asInstanceOf[UITimingConf])

  def parameterEdit(siteEntityRefs: Seq[UISiteEntity])(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    super.parameterEdit() ++ Seq(
      siteEntityRef(uiEntity(siteComp))) ++ siteEntityRefs
      .map(siteEntityRef)
  }

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    parameterEdit(siteConfRefs ++ filterTagConf.toSeq ++ timingConf.toSeq)


  override val hideMenuCreateFrom = true

  // all links to the right - e.g. REGION > PLAYLIST > MEDIUM
  def withLinkedConfRight(siteLevel: UISiteLevel): Set[SiteEntityTrait] = {
    siteConfRefs.flatMap(e => e.withLinkedConfRight(siteLevel)).toSet + siteEntity
  }

  // all links to the left - e.g. REGION > LAYOUT > PLAYER
  def withLinkedConfLeft(siteLevel: UISiteLevel): Set[SiteEntityTrait] = {
    def inner(leftType: SiteType): Set[SiteEntityTrait] =
      (for {
        el <- siteLevel.uiSiteEntities(leftType).uiSiteEntities.map(_.asInstanceOf[UISiteConf])
        er <- el.siteConfRefs
        if er.ident == ident
      } yield el.withLinkedConfLeft(siteLevel) + el.siteEntity)
        .flatten.toSet

    siteType match {
      case LAYOUT => inner(PLAYER)
      case REGION => inner(LAYOUT)
      case PLAYLIST => inner(REGION)
      case MEDIUM => inner(PLAYLIST)
      case _ => Set()

    }
  }

}

object UISiteConf {
}

case class UIPlayerConf(siteEntity: PlayerConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UIPlayer {

  override def hideMenuCreateRegion = true

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

}

case class UILayoutConf(siteEntity: LayoutConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UILayout {

  lazy val layoutComp: LayoutComp = siteEntity.siteInfo.comp

  override val menuItemLink = "link Layout to Player"
  override val linkToType: Option[SiteType] = Some(PLAYER)

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

  override def createPreview(): VNode = {
    UILayout.createPreview(siteEntity, siteEntity.siteConfRefs.map(uiEntity).map(_.asInstanceOf[UIRegion]))
  }
}

case class UIRegionConf(siteEntity: RegionConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UIRegion {

  override val menuItemLink = "link Region to Layout"
  override val linkToType: Option[SiteType] = Some(LAYOUT)

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


}

case class UIPlaylistConf(siteEntity: PlaylistConf
                          , isFiltered: Boolean = false)
  extends UISiteConf
    with UIPlaylist {

  override val menuItemLink = "link Playlist to Region"
  override val linkToType: Option[SiteType] = Some(REGION)

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)


  val insertCanvas = Sink.create[Element] { current =>
    var htmlCanvas = current.asInstanceOf[Canvas]

    val totalDuration = siteEntity.calcDurationInMS()
    val ratio = 1600 / totalDuration

    println(s"insertCanvas: ${siteEntity.ident} ${window.innerHeight}")
    //  val htmlCanvas = current.asInstanceOf[html.Canvas]
    val renderer = htmlCanvas.getContext("2d")
      .asInstanceOf[CanvasRenderingContext2D]

    println(s"update canvas: ${current.id}: ${current.clientWidth} - ${current.clientHeight}")

    htmlCanvas.width = 1000
    htmlCanvas.height = 800
    println(s"update canvas2: ${htmlCanvas.width} - ${htmlCanvas.height}")

    renderer.fillStyle = "white"
    renderer.fillRect(0, 0, 1600, 1000)
    renderer.fillStyle = "blue"
    renderer.strokeStyle = "1px grey"
    renderer.rect(0, 200, totalDuration * ratio, 10)
    renderer.stroke()
    siteEntity.siteConfRefs.foldLeft(0L) { (duration, mc) =>
      println(s"duration: $duration")
      renderer.rect(duration * ratio, 250, mc.durationInMs * ratio, 100)
      renderer.stroke()
      duration + mc.durationInMs
    }

  }

  override def createPreview(): VNode = {
    val totalDuration = siteEntity.calcDurationInMS()
    val ratio = 1600 / totalDuration
    val divHeight = 200

    div(children <-- scaleHandler.map { scale =>
      siteEntity.siteConfRefs.map { mc =>
        println(s"mc: ${mc.ident}")
        val divWidth: Int = (ratio * mc.durationInMs.scaled(scale)).toInt
        div(className := "playlist-medium-preview"
          , css.customStyle(
            s"""
          width: ${divWidth}px;
          height: ${divHeight}px;
           """)
          , img(src := mc.siteComp.url
            , css.customStyle(
              s"""
           max-width: ${divWidth - 4}px;
           max-height: ${divHeight - 24}px;
           """)
          ), div(className := "medium-duration"
            , s"${mc.durationInSecStr}s"
          )
        )
      }

    })
  }
}


case class UIMediumConf(siteEntity: MediumConf
                        , isFiltered: Boolean = false)
  extends UISiteConf
    with UIMedium {

  override val menuItemLink = "link Medium to Playlist"
  override val linkToType: Option[SiteType] = Some(PLAYLIST)

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] =
    parameterEdit(
      siteConfRefs ++ filterTagConf.toSeq ++ timingConf.toSeq) :+
      inputText("Duration", siteEntity.durationInSecStr, Some(siteEntity.durationInSecStr))

  def filter(isFiltered: Boolean): UISiteEntity = copy(isFiltered = isFiltered)

  override def createPreview(): VNode = {
    UIMediumComp(siteEntity.siteComp).createPreview()
  }
}































