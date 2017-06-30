package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State}
import org.scalajs.dom.{CanvasRenderingContext2D, Element, document, html}
import outwatch.Sink
import outwatch.dom._

/**
  * Created by pascal.mengelt on 20.06.2017.
  */
object DetailViewer {


  def apply()(implicit store: ReduxStore[State, Action]): VNode = {

    val updateCanvas = Sink.create[(Element, Element)] { case (old, current) =>
      var htmlCanvas = document.createElement("canvas").asInstanceOf[html.Canvas]
      current.appendChild(htmlCanvas)
      current.setAttribute("width", "100%")

      //  val htmlCanvas = current.asInstanceOf[html.Canvas]
      val renderer = htmlCanvas.getContext("2d")
        .asInstanceOf[CanvasRenderingContext2D]

      println(s"update canvas: ${current.id}: ${current.clientWidth} - ${current.clientHeight}")

      htmlCanvas.width = 1000
      htmlCanvas.height = 800
      println(s"update canvas2: ${htmlCanvas.width} - ${htmlCanvas.height}")

      renderer.fillStyle = "black"
      renderer.fillRect(0, 0, 1200, 1000)
    }

    div(className := bss.grid.row + " detail-viewer full-height"
      , EntityDetailView(bss.grid.col5)
      , child <-- store.map { st =>
        st.selectedSET.map(set =>
        div(className := bss.grid.col7 + " full-height full-width"
          , id := "detailCanvas"
          , update --> updateCanvas
        )).getOrElse("")
      }
    )

  }
}
