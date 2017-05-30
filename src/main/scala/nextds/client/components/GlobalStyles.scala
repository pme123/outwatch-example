package nextds.client.components

import nextds.client.components.Icon.Icon
import nextds.entity._

import scalacss.Defaults._

object GlobalStyles extends StyleSheet.Inline {

  import dsl._

  style(unsafeRoot("body")(
    paddingTop(70.px)
    , height(100.vh)
  ))

  val maxHeight: StyleA = style(
    height(100.%%)
  )

  def siteTypeIcon(siteType: SiteType): Icon =
    Icon(siteType.logo)

  def siteTypeStyle(siteType: SiteType): StyleA = {
    siteType match {
      case PLAYER => playerStyle
      case LAYOUT => layoutStyle
      case REGION => layoutStyle
      case PLAYLIST => playlistStyle
      case MEDIUM => mediumStyle
      case TIME_FILTER => timeFilterStyle
      case TAG_FILTER => tagFilterStyle
    }
  }

  val playerStyle: StyleA = style(
    backgroundColor(rgba(139, 0, 0, .1))
    , borderColor(rgba(139, 0, 0, .2))
  )
  val layoutStyle: StyleA = style(
    backgroundColor(rgba(0, 0, 139, .1))
    , borderColor(rgba(0, 0, 139, .2))
  )
  val regionStyle: StyleA = style(
    backgroundColor(rgba(0, 0, 39, .1))
    , borderColor(rgba(0, 0, 39, .2))
  )
  val playlistStyle: StyleA = style(
    backgroundColor(rgba(0, 100, 0, .1))
    , borderColor(rgba(0, 100, 0, .2))
  )
  val mediumStyle: StyleA = style(
    backgroundColor(rgba(47, 79, 79, .1))
    , borderColor(rgba(47, 79, 79, .2))
  )
  val timeFilterStyle: StyleA = style(
    backgroundColor(rgba(216, 216, 216, .1))
    , borderColor(rgba(216, 216, 216, .2))
  )
  val tagFilterStyle: StyleA = style(
    backgroundColor(rgba(216, 216, 216, .1))
    , borderColor(rgba(216, 216, 216, .2))
  )


  def levelTypeStyle(levelType: LevelType): StyleA = {

    levelType match {
      case TEMPL => templStyle
      case COMP => compStyle
      case CONF => confStyle
      case FILTER => filterStyle
    }
  }

  val templStyle: StyleA = style(
    backgroundColor(rgba(173, 216, 230, .1))
    , borderColor(rgba(173, 216, 230, .4))
  )
  val compStyle: StyleA = style(
    backgroundColor(rgba(144, 238, 144, .1))
    , borderColor(rgba(144, 238, 144, .4))
  )
  val confStyle: StyleA = style(
    backgroundColor(rgba(255, 182, 193, .1))
    , borderColor(rgba(255, 182, 193, .4))
  )
  val filterStyle: StyleA = style(
    backgroundColor(rgba(216, 216, 216, .1))
    , borderColor(rgba(216, 216, 216, .4))
  )

  val logo: StyleA = style(
    marginRight(20.px)
    , padding(2.px)
    , width(50.px)
  )

  val projectName: StyleA = style(
    marginRight(120.px)
  )

  val levelDiv: StyleA = style(
    width(100.%%)
    , marginLeft(5.px)
    , marginRight(5.px)
    , boxShadow := "0 1px 3px 0 rgba(0, 0, 0, 0.12), 0 1px 2px 0 rgba(0, 0, 0, 0.12)"
  )

  val mainCell: StyleA = style(
    height(100.%%)
    , display.tableCell
    , padding(0.px)
  )

  val panelInnerDiv: StyleA = style(
    width(100.%%)
    , padding(5.px)
    , margin(0.px)
  )

  val siteEntityDiv: StyleA = style(

  )

  val siteEntityUL: StyleA = style(
    margin(0.px)
    , float.left
    , width(98.%%)
  )

  val siteEntityLI: StyleA = style(
    padding(0.px)
  )

  val siteEntityElem: StyleA = style(
    padding(4.px)
  )

  val siteEntityIcon: StyleA = style(
    float.left
  )

  val siteEntityIdent: StyleA = style(
    fontWeight.bold
    , float.left
  )

  val siteEntityTitle: StyleA = style(
    fontStyle.italic
    , opacity(.6)
    , float.left
    , textOverflow := "ellipsis"
    , whiteSpace.nowrap
    , overflow.hidden
    , fontSize.small
  )

  val siteEntityMenuIcon: StyleA = style(
    background := transparent
    , padding(0.px)
  )

  val siteEntityMenu: StyleA = style(
  )

  val siteEntityMenuItem: StyleA = style(
    padding(0.px, 6.px)
    , cursor.pointer
  )

  val chosenEntity: StyleA = style(
    backgroundColor(rgba(255, 255, 0, .3))
  )

  def styleClassNames(styles: StyleA*): String =
    styles.toSeq.map(_.htmlClass).mkString(" ")

  val bootstrapStyles = new BootstrapStyles
}
