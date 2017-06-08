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

  def siteEntity: SiteEntityTrait

  def levelType: LevelType = siteEntity.levelType

  def siteType: SiteType = siteEntity.siteType

  val stylesMenu: String =  bss.dropdown.menu

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
        , hidden := hideMenuItem
        , menuItemCreate)
      // element for special region
      , li(className := stylesMenuItem
        , click(CreateFrom(siteEntity, isRegion = true)) --> store
        , hidden := hideMenuItem2
        , menuItemCreate2)
      , li(role := "separator"
        , className := "divider")
      , li(className := stylesMenuItem
        , s"delete ${siteEntity.ident}")
    )

  def menuItemCreate = ""

  def menuItemCreate2 = s"create ${REGION.label} ${CONF.label}"

  def hideMenuItem = false

  def hideMenuItem2 = true

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
