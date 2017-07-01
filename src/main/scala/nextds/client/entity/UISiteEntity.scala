package nextds.client.entity

import nextds.entity._
import org.scalajs.dom.CanvasRenderingContext2D
import outwatch.dom.{style, _}


case class UISiteEntities(
                           siteEntities: SiteEntities[_ <: SiteEntityTrait]
                           , uiSiteEntities: Seq[UISiteEntity]
                         ) {

  def entity(indexFrom: Int): UISiteEntity =
    uiSiteEntities(indexFrom)

  def entity(ident: String): UISiteEntity =
    uiSiteEntities
      .find(_.siteEntity.ident == ident)
      .getOrElse(throw new IllegalArgumentException(s"There is no SiteEntity with ident: $ident"))

  def appendFilter(filters: UIFilters): UISiteEntities = {
    copy(uiSiteEntities = uiSiteEntities.map(_.appendFilter(filters)))
  }

  def replaceEntity(set: UISiteEntity): UISiteEntities =
    copy(uiSiteEntities = uiSiteEntities.filter(_.ident != set.ident) :+ set)

}

object UISiteEntities {

}

/**
  * Created by pascal.mengelt on 28.05.2017.
  */
trait UISiteEntity
  extends UIElements {

  def isFiltered: Boolean

  def siteEntity: SiteEntityTrait

  def levelType: LevelType = siteEntity.levelType

  def siteType: SiteType = siteEntity.siteType

  def ident: SiteEntityIdent = siteEntity.ident

  def label: SiteEntityIdent = siteEntity.label

  def siteIdent: SiteIdent = siteEntity.siteIdent

  def title: String = siteEntity.title

  def linkToType: Option[SiteType] = None

  val stylesMenu: String = bss.dropdown.menu

  val stylesMenuItem: String =
    css.siteEntityMenuItem

  def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    Seq(
      inputText("Title", siteEntity.title, siteEntity.maybeTitle)
      , inputDescription
    )
  }

  def inputDescription: VNode = {
    inputTextarea("Description", siteEntity.descr, siteEntity.maybeDescr)
  }

  def createMenu()(implicit store: ReduxStore[State, Action]): VNode =
    ul(className := stylesMenu
      , li(className := stylesMenuItem
        , click(Edit(this)) --> store
        , s"edit ${siteEntity.ident}")
      , li(className := stylesMenuItem
        , click(CreateFrom(siteEntity)) --> store
        , hidden := hideMenuCreateFrom
        , menuItemCreateFrom)
      // element for special region
      , li(className := stylesMenuItem
        , click(CreateFrom(siteEntity, isRegion = true)) --> store
        , menuItemCreateRegion
        , hidden := hideMenuCreateRegion
      )
      // element for add Config
      , li(className := stylesMenuItem
        , hidden := hideMenuLink
        , button(`type` := "button"
          , data.toggle := "modal"
          , data.target := "#modalDialog"
          , click(LinkTo(this)) --> store
          , menuItemLink
        )
      )
      , li(role := "separator"
        , className := "divider")
      , li(className := stylesMenuItem
        , s"delete ${siteEntity.ident}")
    )

  def menuItemCreateFrom = ""

  def menuItemCreateRegion = s"create ${REGION.label} ${CONF.label}"

  def menuItemLink = ""

  def hideMenuCreateFrom = false

  def hideMenuCreateRegion = true

  def hideMenuLink = false

  def appendFilter(filters: UIFilters): UISiteEntity = {
    val identMatch = filters.ident.forall(i => ident.toLowerCase.contains(i.toLowerCase))
    val titleMatch = filters.title.forall(t => title.toLowerCase.contains(t.toLowerCase))
    val siteMatch = filters.sites match {
      case Some(Nil) | None => true
      case Some(levels) => levels.contains(siteIdent)
    }
    val levelTypeMatch = filters.levels match {
      case Some(Nil) | None => true
      case Some(levelTypes) => levelTypes.contains(levelType)
    }
    val siteTypeMatch = filters.siteTypes match {
      case Some(Nil) | None => true
      case Some(siteTypes) => siteTypes.contains(siteType)
    }
    // println(s"appendFilter: $siteIdent - ident: $identMatch - title: $titleMatch sites: $siteMatch - levelTypes: $levelTypeMatch - siteTypeMatch: $siteTypeMatch")

    filter(!(identMatch && titleMatch && siteMatch && levelTypeMatch && siteTypeMatch))
  }

  protected def filter(isFiltered: Boolean): UISiteEntity

  def createPreview(): VNode = {
    "no preview yet"
  }

  def drawPrieview(renderer: CanvasRenderingContext2D) {
    // nothing by default
  }


}

trait UIPlayer extends UISiteEntity {
  def siteEntity: PlayerTrait

}

trait UILayout extends UISiteEntity {
  def siteEntity: LayoutTrait

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    super.parameterEdit() ++
      Seq(
        inputNumber("From Left"
          , siteEntity.screenRegion.fromLeft.toString
          , siteEntity.maybeScreenRegion.map(_.fromLeft))
        , inputNumber("From Top"
          , siteEntity.screenRegion.fromTop.toString
          , siteEntity.maybeScreenRegion.map(_.fromLeft))
        , inputNumber("Width"
          , siteEntity.screenRegion.width.toString
          , siteEntity.maybeScreenRegion.map(_.width))
        , inputNumber("From Left"
          , siteEntity.screenRegion.height.toString
          , siteEntity.maybeScreenRegion.map(_.height))
      )
  }

  override def createPreview(): VNode = {
    val screenRegion = siteEntity.screenRegion
    val scaledRegion = (screenRegion.fromLeft.scaled, screenRegion.fromTop.scaled, screenRegion.width.scaled, screenRegion.height.scaled)
    div(className := "preview-div " + css.siteTypeStyle(siteType)
      , Attribute("style",
        s"""
          left: ${scaledRegion._1}px;
          top: ${scaledRegion._2}px;
          width: ${scaledRegion._3}px;
          height: ${scaledRegion._4}px;
           """)
      , bss.tooltip.divWithSimple(title, "preview-div-title")
        , bss.tooltip.divWithSimple(screenRegion.print(), "preview-div-subtitle")
    )
  }

  override def drawPrieview(renderer: CanvasRenderingContext2D): Unit = {
    val screenRegion = siteEntity.screenRegion
    renderer.fillStyle = "gray"
    renderer.font = "12px sans-serif"
    renderer.textAlign = "center"
    renderer.textBaseline = "middle"

    val scaledRegion = (screenRegion.fromLeft.scaled, screenRegion.fromTop.scaled, screenRegion.width.scaled, screenRegion.height.scaled)
    val center = (scaledRegion._3 / 2 + scaledRegion._1, scaledRegion._4 / 2 + scaledRegion._2)
    val titleWidth = Math.max(renderer.measureText(title).width, scaledRegion._3)
    println(s"titleWidth $titleWidth")
    renderer.fillText(title, center._1, center._2 - 10, scaledRegion._3)
    renderer.font = "8px"
    renderer.fillText(screenRegion.print(), center._1, center._2 + 10, scaledRegion._3)
    renderer.rect(scaledRegion._1, scaledRegion._2, scaledRegion._3, scaledRegion._4)
    renderer.stroke()
  }

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
