package nextds.client.components

import nextds.client.components.Icon.Icon
import nextds.entity._


object GlobalStyles {

  def siteTypeIcon(siteType: SiteType): Icon =
    Icon(siteType.logo)

  def siteTypeStyle(siteType: SiteType): String = {
    siteType match {
      case PLAYER => playerStyle
      case LAYOUT => layoutStyle
      case REGION => regionStyle
      case PLAYLIST => playlistStyle
      case MEDIUM => mediumStyle
      case TIMING => timingStyle
      case FILTER_TAG => tagFilterStyle
    }
  }

  val playerStyle = "player-style"
  val layoutStyle = "layout-style"
  val regionStyle = "region-style"
  val playlistStyle = "playlist-style"
  val mediumStyle = "medium-style"
  val tagFilterStyle = "tag-filter-style"
  val timingStyle = "timing-style"


  def levelTypeStyle(levelType: LevelType): String =
    levelType match {
      case TEMPL => templStyle
      case COMP => compStyle
      case CONF => confStyle
      case FILTER => filterStyle
      case TIME => timeStyle
    }

  val templStyle = "templ-style"
  val compStyle = "comp-style"
  val confStyle = "conf-style"
  val filterStyle = "filter-style"
  val timeStyle = "time-style"

  def siteLevelDiv(levelType: LevelType): String = Seq(
    css.levelTypeStyle(levelType)
    , "level-style"
    , bss.panel.standard
  ) mkString " "

  def siteLevelInnerDiv(levelType: LevelType): String = Seq(
    css.levelDiv
    , css.levelTypeStyle(levelType)
    , css.panelInnerDiv
    , bss.panel.row
  ) mkString " "

  val logo = "logo"
  val projectName = "project-name"
  val levelDiv = "level-div"
  val mainCell = "main-cell"
  val panelInnerDiv = "panel-inner-div"

  def siteEntitiesDiv(levelType:LevelType, siteType:SiteType): String =
    (levelType, siteType) match {
      case (CONF | TIME, REGION | PLAYLIST | LAYOUT | TIMING) => bss.grid.col2
      case (_, PLAYLIST) => bss.grid.col2
      case (_,LAYOUT) => bss.grid.col4
      case (_, _) => bss.grid.col3
    }

  val siteEntitiesUL: String =  Seq(
    "site-entity-ul"
    , bss.listGroup.listGroup
  ) mkString " "

  val siteEntityLI = "site-entity-li"
  val siteEntityElem = "site-entity-elem"
  val siteEntityMenuIcon = "site-entity-menu-icon"
  val siteEntityMenuItem = "site-entity-menu-item"

  val chosenEntity = "chosen-entity"

  def entityCardLI(siteType: SiteType): String = Seq(
    bss.listGroup.item
    , bss.grid.row
    , siteTypeStyle(siteType)
    , "site-entity-li"
  ) mkString " "

  val entityCardIcon: String = Seq(
    "site-entity-icon"
    , siteEntityElem
    , bss.grid.col1
  ) mkString " "

  val entityCardIdent: String = Seq(
    "site-entity-ident"
    , siteEntityElem
    , bss.grid.col10
  ) mkString " "

  val entityCardMenu: String = Seq(
    siteEntityMenuIcon
    , siteEntityElem
    , bss.grid.col1
  ) mkString " "

  val entityCardTitle: String = Seq(
    "site-entity-title"
    , siteEntityElem
    , bss.grid.col12
  ) mkString " "


  def markerColorRun(status: PlayerStatus): String = status match {
    case PlayerStatus.RUNNING => "#16631D"
    case PlayerStatus.NOT_CONNECTED => "#B92F2A"
    case PlayerStatus.STOPPED => "#E59C2A"
  }
}
