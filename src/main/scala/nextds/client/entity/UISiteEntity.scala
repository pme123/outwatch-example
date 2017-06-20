package nextds.client.entity

import nextds.client.components.{BootstrapStyles, GlobalStyles}
import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 28.05.2017.
  */
trait UISiteEntity
  extends UIElements {

  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles

  def isFiltered: Boolean

  def siteEntity: SiteEntityTrait

  def levelType: LevelType = siteEntity.levelType

  def siteType: SiteType = siteEntity.siteType

  def ident: SiteEntityIdent = siteEntity.ident

  def siteIdent: SiteIdent = siteEntity.siteIdent

  def title: String = siteEntity.title

  def linkToType: Option[SiteType] = None

  val stylesMenu: String = bss.dropdown.menu

  val stylesMenuItem: String =
    css.siteEntityMenuItem

  def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    Seq(
      inputText("Title", siteEntity.title, siteEntity.maybeTitle)
      , inputTextarea("Description", siteEntity.descr, siteEntity.maybeDescr)
    )
  }

  def createMenu()(implicit store: ReduxStore[State, Action]): VNode =
    ul(className := stylesMenu
      , li(className := stylesMenuItem
        , click(Edit(siteEntity)) --> store
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
        , button(`type` := "button"
          , data.toggle := "modal"
          , data.target := "#modalDialog"
          , click(LinkTo(this)) --> store
          , menuItemLink
        )
        , hidden := hideMenuLink
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
