package nextds.client

import net.scalapro.sortable.{EventS, Sortable, SortableProps}
import nextds.client.components.{EntityCard, EntityDetailView, GlobalStyles, Icon}
import nextds.client.entity._
import nextds.entity._
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLDivElement
import outwatch.dom._
import rxscalajs.Observable

import scala.scalajs.js
import scala.scalajs.js.{JSApp, UndefOr}
import scalacss.Defaults._

/**
  * Created by pascal.mengelt on 17.05.2017.
  */
object NextDSApp extends JSApp {

  def main(): Unit = {
    val nextDS = NextDS()
    OutWatch.render("#app", nextDS.root)
    // create stylesheet
    GlobalStyles.addToDocument()
    EntityDetailView.Style.addToDocument()
    UIElements.Style.addToDocument()

    nextDS.addSorting()

  }
}

case class NextDS() {

  @inline private def bss = GlobalStyles.bootstrapStyles

  @inline private def css = GlobalStyles


  implicit val store: ReduxStore[State, Action] = ReduxStore()

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
          , children <-- entities)
      )


  }

  val fs: js.Function1[EventS, Unit] = (event) => {
    val from = event.from.asInstanceOf[HTMLDivElement]
    val item = event.item.asInstanceOf[HTMLDivElement]
    val to = event.to.asInstanceOf[HTMLDivElement]
    println(event.`type`)
    println(s"${from.id} > ${to.id}:: ${item.id} - ${event.oldIndex}<>${event.newIndex}")
  }

  val pull: js.Function2[Sortable, Sortable, js.Any] = { (to: Sortable, from: Sortable) => {
    println("PULL")

    from.el.children.length match {
      case x if x > 2 => true
      case _ => "clone"
    }
  }
  }

  val put: js.Function1[Sortable, js.Any] = { (to: Sortable) =>
    println("PUT")
    to.el.children.length < 4
  }

  def addSorting(): Sortable = {
    def prop = new SortableProps {
      override val group = js.Dictionary(
        "name" -> "TEMPL-PLAYER",
        "pull" -> pull

      )
      override val animation = 500
      override val handle = ".glyphicon-move"
    }

    def prop2 = new SortableProps {
      override val group = js.Dictionary(
        "name" -> "COMP-PLAYER"
        ,        "put" -> put

      )
      override val animation = 500
      override val handle = ".glyphicon-move"
      override val onAdd: UndefOr[js.Function1[EventS, Unit]] = fs
    }

    Sortable(dom.document.getElementById("TEMPL-PLAYER"), prop)
    Sortable(dom.document.getElementById("COMP-PLAYER"), prop2)
  }

  val root: VNode =
    div(
      div(className := bss.grid.row.htmlClass
        , div(className := bss.grid.col9.htmlClass
          , div(className := bss.grid.row.htmlClass
            // , div(className := "col-sm-10"
            , children <-- listViews
          ))
        , EntityDetailView()
      )
    )
}

