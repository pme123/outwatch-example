package nextds.client.components

import google.maps._
import nextds.client.entity.{Action, ReduxStore, State, UIPlayerComp}
import nextds.entity.{COMP, PLAYER, PlayerComp, SiteEntityIdent}
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

  def infoWindow(playerComp: PlayerComp) = {
    val contentString =
      s"""
        <div id="${playerComp.ident}-info">
            <h1 id="firstHeading" class="firstHeading">${playerComp.ident}</h1>
            <p>${playerComp.title}</p>
            <p>${playerComp.maybeLocation.get}</p>
        </div>
       """
    new google.maps.InfoWindow(google.maps.InfoWindowOptions(
      content = contentString
    ))
  }

  def apply()(implicit store: ReduxStore[State, Action]): VNode = {


    val players =
      store.map { st =>
        val selectedIdent = st.selectedSET.map(_.ident).getOrElse("-")
        val players = st.siteModel.uiSiteEntities(COMP, PLAYER)
          .uiSiteEntities
          .asInstanceOf[Seq[UIPlayerComp]]

        (players, selectedIdent)
      }

    val selectPlayer = store.map(_.selectedSET)

    def initialize(elem: Element)() = js.Function {
      val opts = MapOptions(
        center = new LatLng(47.056856, 8.539656700000023),
        zoom = 8,
        panControl = false,
        streetViewControl = false,
        mapTypeControl = false)
      val gmap = new google.maps.Map(elem, opts)
      players.subscribe(pls => pls._1.foreach { uie =>
        val pl = uie.siteEntity
        pl.maybeLocation.foreach { loc =>
          val marker = new Marker(MarkerOptions(
            position = new LatLng(loc.lat, loc.lng),
            map = gmap,
            title = s"${pl.ident}: ${pl.title}"
            , clickable = true
            , icon = js.Dynamic.literal(
              path = SymbolPath.FORWARD_CLOSED_ARROW
              , rotation = 90
              , fillColor = css.markerColorRun(pl.status)
              , fillOpacity = 1
              , strokeWeight = 1
              , scale = 5
            ).asInstanceOf[MarkerShape]
          ))

          val window1 = infoWindow(pl)
          selectPlayer.subscribe(set => set match {
            case Some(selPl: UIPlayerComp) if selPl.ident == pl.ident =>
              window1.open(gmap, marker)
            case _ => window1.close()
          })
          google.maps.event.addListener(marker, "click", () => {
            window1.open(gmap, marker)
          })
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
        , ul(className := css.siteEntitiesUL
          , children <-- players.map(pls => pls._1.map(EntityCard(_, pls._2))))
      ))
  }
}
