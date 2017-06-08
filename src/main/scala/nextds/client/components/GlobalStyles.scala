package nextds.client.components

import nextds.client.components.Icon.Icon
import nextds.entity._


object GlobalStyles {


  def siteTypeIcon(siteType: SiteType): Icon =
    Icon(siteType.logo)

  def siteTypeStyle(siteType: SiteType) = {
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

  val playerStyle = "player-style"
  val layoutStyle = "layout-style"
  val regionStyle = "region-style"
  val playlistStyle = "playlist-style"
  val mediumStyle = "medium-style"
  val tagFilterStyle = "tag-filter-style"
  val timeFilterStyle = "time-filter-style"


  def levelTypeStyle(levelType: LevelType) = {

    levelType match {
      case TEMPL => templStyle
      case COMP => compStyle
      case CONF => confStyle
      case FILTER => filterStyle
    }
  }

  val templStyle = "templ-style"
  val compStyle = "comp-style"
  val confStyle = "conf-style"
  val filterStyle = "filter-style"

  val logo = "logo"
  val projectName = "project-name"
  val levelDiv = "level-div"
  val mainCell = "main-cell"
  val panelInnerDiv = "panel-inner-div"

  val siteEntityUL = "site-entity-ul"
  val siteEntityLI = "site-entity-li"
  val siteEntityElem = "site-entity-elem"
  val siteEntityIcon = "site-entity-icon"
  val siteEntityIdent = "site-entity-ident"
  val siteEntityTitle = "site-entity-title"
  val siteEntityMenuIcon = "site-entity-menu-icon"
  val siteEntityMenuItem = "site-entity-menu-item"

  val chosenEntity = "chosen-entity"

}
