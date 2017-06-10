package nextds.client

import net.scalapro.sortable.{EventS, Sortable, SortableProps}
import nextds.client.components._
import nextds.client.entity._
import nextds.entity._
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLDivElement
import outwatch.Sink
import outwatch.dom._
import outwatch.dom.helpers.EventEmitterBuilder
import rxscalajs.Observable

import scala.scalajs.js
import scala.scalajs.js.UndefOr

/**
  * Created by pascal.mengelt on 17.05.2017.
  */
object NextDSApp extends js.JSApp {

  def main(): Unit = {
    val nextDS = NextDS()
    OutWatch.render("#app", nextDS.root)
    nextDS.addSorting()
  }
}

case class NextDS() {

  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles


  implicit val store: ReduxStore[State, Action] = ReduxStore()

  val listViews: Observable[Seq[VNode]] = store
    .map(
      _.siteModel.allLevels
        .map(m => levelComponent(m.levelType))
    )

  def levelComponent(levelType: LevelType): VNode = {

    val stylesDiv1 = Seq(
      css.levelTypeStyle(levelType)
      , bss.panel.standard
    ) mkString " "
    val stylesDiv2 = Seq(
      css.levelDiv
      , css.levelTypeStyle(levelType)
      , css.panelInnerDiv
      , bss.panel.row
    ) mkString " "

    val siteLevel = {
      store.map(
        _.siteModel.level(levelType)
          .allSiteTypes
          .filterNot(siteType => levelType != CONF && siteType == REGION)
          .map(siteType =>
            entityListComponent(levelType, siteType))
      )
    }

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

    val stylesDiv =
      (levelType, siteType) match {
        case (CONF, REGION | PLAYLIST | MEDIUM) => bss.grid.col2
        case (_, _) => bss.grid.col3
      }

    val stylesUL = Seq(
      css.siteEntityUL
      , bss.listGroup.listGroup
    ) mkString " "

    div(className := stylesDiv
      , ul(id := s"$levelType-$siteType"
        , className := stylesUL
        , children <-- entities)
    )

  }

  lazy val dragEmitter = new EventEmitterBuilder("dragEnd")
  def runOnEnd(from: String, to: String): js.Function1[EventS, Unit] = (event) => {
    if (!js.isUndefined(event) && !js.isUndefined(event.oldIndex)) {
      store <-- Observable.create(obs => obs.next(CreateFromDrag(from, to, event.oldIndex.asInstanceOf[Int])))
    }
  }


  def addSorting(): Sortable = {

    def prop = new SortableProps {
      override val group = js.Dictionary(
        "name" -> "TEMPL-PLAYER"
        , "pull" -> "clone" //pull("TEMPL-PLAYER", "COMP-PLAYER")

      )
      override val sort = false
      override val animation = 200
      override val handle = s".${Icon.dragHandle}"
      //override val onClone: UndefOr[js.Function1[EventS, Unit]] = fs("TEMPL-PLAYER", "COMP-PLAYER")
      override val onEnd: UndefOr[js.Function1[EventS, Unit]] = runOnEnd("TEMPL-PLAYER", "COMP-PLAYER")
      // override val onStart: UndefOr[js.Function1[EventS, Unit]] = fs("TEMPL-PLAYER", "COMP-PLAYER")
    }

    def prop2 = new SortableProps {
      override val group = js.Dictionary(
        "name" -> "COMP-PLAYER"
        , "put" -> js.Array("TEMPL-PLAYER")
      )
      override val sort = false
      override val animation = 200
    }

    Sortable(dom.document.getElementById("TEMPL-PLAYER"), prop)
    Sortable(dom.document.getElementById("COMP-PLAYER"), prop2)

  }

  val root: VNode =
    div(
      div(className := bss.grid.row
        , div(className := bss.grid.col9
          , div(className := bss.grid.row
            // , div(className := "col-sm-10"
            , children <-- listViews
          ))
        , EntityDetailView()
      )
    )
}

