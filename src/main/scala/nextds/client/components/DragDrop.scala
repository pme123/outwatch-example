package nextds.client.components

import net.scalapro.sortable.{EventS, Sortable, SortableProps}
import nextds.client.entity._
import nextds.entity._
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLDivElement
import rxscalajs.Observable

import scala.scalajs.js
import scala.scalajs.js.UndefOr

/**
  * Created by pascal.mengelt on 12.06.2017.
  */
case class DragDrop(implicit store: ReduxStore[State, Action]) {
  private val animationMS = 200
  private val handleClass = s".${Icon.dragHandle}"
  private val sortFlag = false

  private def runOnEnd(from: String, to: String): js.Function1[EventS, Unit] = (event) => {
    if (!js.isUndefined(event) && !js.isUndefined(event.oldIndex)) {
      store <-- Observable.create(obs => obs.next(CreateFromDrag(from, to
        , event.oldIndex.asInstanceOf[Int])))
    }
  }

  private def runOnEndConf(from: String, to: String): js.Function1[EventS, Unit] = (event) => {
    val froms = event.from.asInstanceOf[HTMLDivElement]
    val items = event.item.asInstanceOf[HTMLDivElement]
    val toElem = dom.document.getElementById(to)
    val newChildren = toElem.removeChild(toElem.children.namedItem(items.id))
    println(s"newChildren: ${newChildren.textContent}")
    println(s"${froms.id} > ${toElem.id}:: ${items.id} - ${event.oldIndex}<>${event.newIndex}")

    println(event.`type`)
    if (!js.isUndefined(event) && !js.isUndefined(event.oldIndex)) {
      /*store <-- Observable.create(obs => obs.next(CreateFromDrag(from, to
        , event.oldIndex.asInstanceOf[Int]
        , Some(event.newIndex.asInstanceOf[Int])))) */
    }
  }


  private def addTemplSource(siteType: SiteType): Sortable = {

    def prop = new SortableProps {
      override val group = js.Dictionary(
        "name" -> s"$TEMPL-$siteType"
        , "pull" -> false

      )
      override val sort: js.UndefOr[Boolean] = sortFlag
      override val animation: js.UndefOr[Int] = animationMS
      override val handle: js.UndefOr[String] = handleClass
      override val onEnd: UndefOr[js.Function1[EventS, Unit]] = runOnEnd(s"$TEMPL-$siteType", s"$COMP-$siteType")
    }
    Sortable(dom.document.getElementById(s"$TEMPL-$siteType"), prop)
  }

  private def addCompTargetSource(siteType: SiteType): Sortable = {

    def prop = new SortableProps {
      override val group = js.Dictionary(
        "name" -> s"$COMP-$siteType"
        , "pull" -> false
        , "put" -> js.Array(s"$TEMPL-$siteType")
      )
      override val sort: js.UndefOr[Boolean] = sortFlag
      override val animation: js.UndefOr[Int] = animationMS
      override val handle: js.UndefOr[String] = handleClass
      override val onAdd: UndefOr[js.Function1[EventS, Unit]] = runOnEnd(s"$COMP-$siteType", s"$CONF-$siteType")
    }
    Sortable(dom.document.getElementById(s"$COMP-$siteType"), prop)
  }

  private def addConfPlayer(siteType: SiteType): Sortable = {

    def prop = new SortableProps {
      override val group = js.Dictionary(
        "name" -> s"$CONF-$siteType"
        , "pull" -> false
        , "put" -> js.Array(s"$COMP-$siteType",s"$CONF-$LAYOUT")
      )
      override val sort: js.UndefOr[Boolean] = sortFlag
    }
    Sortable(dom.document.getElementById(s"$CONF-$siteType"), prop)
  }

  private def addConfLayout(siteType: SiteType, siteFrom: SiteType, siteTo: SiteType): Sortable = {

    def prop = new SortableProps {
      override val group = js.Dictionary(
        "name" -> s"$CONF-$siteType"
        , "pull" -> "clone"
        , "put" -> js.Array(s"$COMP-$siteType",s"$CONF-$siteFrom")
      )
      override val sort: js.UndefOr[Boolean] = sortFlag
      override val animation: js.UndefOr[Int] = animationMS
      override val handle: js.UndefOr[String] = handleClass
      override val onEnd: UndefOr[js.Function1[EventS, Unit]] = runOnEndConf(s"$CONF-$siteType", s"$CONF-$siteTo")
    }
    Sortable(dom.document.getElementById(s"$CONF-$siteType"), prop)
  }

  Seq(PLAYER, LAYOUT, PLAYLIST, MEDIUM)
    .map { siteType =>
      addTemplSource(siteType)
      addCompTargetSource(siteType)
    }
 /* addConfPlayer(PLAYER) // not used
  addConfLayout(LAYOUT, REGION, PLAYER)
  addConfLayout(REGION, PLAYLIST, LAYOUT)
  addConfLayout(PLAYLIST, MEDIUM, REGION)
  addConfLayout(MEDIUM, MEDIUM, PLAYLIST) */
}
