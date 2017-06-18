package nextds.client.components

import google.maps.LatLng
import nextds.client.entity.{Action, ReduxStore, State}
import org.scalajs.dom._
import outwatch.Sink
import outwatch.dom._

import scala.scalajs.js

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
object PlayerMonitor {

  def initialize(elem: Element)() = js.Function {
    val opts = google.maps.MapOptions(
      center = new LatLng(47.056856, 8.539656700000023),
      zoom = 8,
      panControl = false,
      streetViewControl = false,
      mapTypeControl = false)
    val gmap = new google.maps.Map(elem, opts)
    ""
  }

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {

    val initMapSink = Sink.create[Element] { e =>
      google.maps.event.addDomListener(window, "load", initialize(e))
    }

    div(className:="monitor-map"
      , insert --> initMapSink)
  }
}
