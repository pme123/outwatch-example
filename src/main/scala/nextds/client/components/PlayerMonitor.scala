package nextds.client.components

import google.maps._
import nextds.client.entity.{Action, ReduxStore, State, UIPlayerComp}
import nextds.entity.{COMP, PLAYER}
import org.scalajs.dom._
import outwatch.Sink
import outwatch.dom._
import rxscalajs.Observable

import scala.scalajs.js

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
object PlayerMonitor {

  @inline private def bss = BootstrapStyles

  @inline private def css = GlobalStyles


  def apply()(implicit store: ReduxStore[State, Action]): VNode = {
    var actPlayers: Seq[UIPlayerComp] = Nil

    val players: Observable[Seq[UIPlayerComp]] = store.map { st =>
      actPlayers = st.siteModel.entities(COMP, PLAYER)
        .asInstanceOf[Seq[UIPlayerComp]]
      actPlayers
    }

    def initialize(elem: Element)() = js.Function {
      val opts = MapOptions(
        center = new LatLng(47.056856, 8.539656700000023),
        zoom = 8,
        panControl = false,
        streetViewControl = false,
        mapTypeControl = false)
      val gmap = new google.maps.Map(elem, opts)
      players.subscribe(_.foreach { pl =>
        pl.siteEntity.maybeLocation.foreach { loc =>
          val marker = new Marker(MarkerOptions(
            position = new LatLng(loc.lat, loc.lng),
            map = gmap,
            title = pl.ident
          ))
        }
      }
      )
      ""
    }

    val initMapSink = Sink.create[Element] { e =>
      google.maps.event.addDomListener(window, "load", initialize(e))
    }

    div(className := bss.grid.row + " full-height"
      , div(className := bss.grid.col9 + " monitor-map"
        , insert --> initMapSink)
      , div(
        className := bss.grid.col3
        , ul(className := css.siteEntityUL
          , children <-- players.map(_.map(EntityCard.apply)))
      ))
  }
}
