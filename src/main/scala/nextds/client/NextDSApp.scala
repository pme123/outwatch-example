package nextds.client

import nextds.client.components.{EntityCard, EntityDetailView, GlobalStyles}
import nextds.client.entity._
import nextds.entity._
import outwatch.dom._
import outwatch.util.Store
import rxscalajs.Observable

import scala.scalajs.js.JSApp
import scalacss.Defaults._

/**
  * Created by pascal.mengelt on 17.05.2017.
  */
object NextDSApp extends JSApp {
  def main(): Unit = {
    OutWatch.render("#app", NextDS().root)
    // create stylesheet
    GlobalStyles.addToDocument()
    EntityDetailView.Style.addToDocument()
    UIElements.Style.addToDocument()
  }
}

case class NextDS() {

  @inline private def bss = GlobalStyles.bootstrapStyles
  @inline private def css = GlobalStyles


  implicit val store: Store[State, Action] = ReduxStore.store()

  val listViews: Observable[Seq[VNode]] = store
    .map(
      _.siteModel.allLevels
        .map(m => levelComponent(m.levelType))
    )

  def levelComponent(levelType: LevelType): VNode = {
    val stylesDiv1 = css.styleClassNames(
      css.levelTypeStyle(levelType)
      , bss.panel.default
    )
    val stylesDiv2 = css.styleClassNames(
      css.levelDiv
      , css.levelTypeStyle(levelType)
      , css.panelInnerDiv
      , bss.panel.row
    )

    val siteLevel =
      store.map(_.siteModel.level(levelType)
        .allSiteTypes
        .filterNot(siteType => levelType != CONF && siteType == REGION)
        .map(siteType =>
          entityListComponent(levelType, siteType))
      )

    div(className := stylesDiv1
      , div(className := stylesDiv2
        , id := s"$levelType"
        , children <-- siteLevel
      ))
  }

  def entityListComponent(levelType: LevelType, siteType: SiteType): VNode = {


    val entities =
      store.map(_.siteModel.entities(levelType, siteType)
        .map(EntityCard.apply))

    val stylesDiv = css.styleClassNames(
      css.siteEntityDiv
      , (levelType, siteType) match {
        case (CONF, REGION | PLAYLIST | MEDIUM) => bss.grid.col2
        case (_, _) => bss.grid.col3
      }
    )
    val stylesUL = css.styleClassNames(
      css.siteEntityUL
      , bss.listGroup.listGroup)

    div(className := stylesDiv
      , ul(id := s"$levelType-$siteType"
        , className := stylesUL
        , li(className := css.siteEntityLI.htmlClass
          , children <-- entities)
      ))

  }


  val root: VNode =
    div(className := bss.grid.row.htmlClass
      , div(className := bss.grid.col9.htmlClass
      , div(className := bss.grid.row.htmlClass
        // , div(className := "col-sm-10"
        , children <-- listViews
      ))
      , EntityDetailView(store))
}

