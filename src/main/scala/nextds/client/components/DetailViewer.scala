package nextds.client.components

import nextds.client.entity.{Action, ReduxStore, State, UISiteEntity}
import org.scalajs.dom
import org.scalajs.dom._
import outwatch.Sink
import outwatch.dom._

/**
  * Created by pascal.mengelt on 20.06.2017.
  */
object DetailViewer {


  def apply()(implicit store: ReduxStore[State, Action]): VNode = {

    var selectedEntity: Option[UISiteEntity] = None

    val updateCanvas = Sink.create[(Element, Element)] { case (old, current) =>
      var htmlCanvas = current.asInstanceOf[html.Canvas]

      println(s"selectedElem: ${selectedEntity} ${window.innerHeight}")
      //  val htmlCanvas = current.asInstanceOf[html.Canvas]
      val renderer = htmlCanvas.getContext("2d")
        .asInstanceOf[CanvasRenderingContext2D]

      println(s"update canvas: ${current.id}: ${current.clientWidth} - ${current.clientHeight}")

      htmlCanvas.width = 1000
      htmlCanvas.height = 800
      println(s"update canvas2: ${htmlCanvas.parentElement.parentElement.clientWidth} - ${htmlCanvas.parentElement.parentElement.parentElement.clientWidth}")

      renderer.fillStyle = "white"
      renderer.fillRect(0, 0, 1200, 1000)

      selectedEntity.foreach{ set =>
        set.drawPrieview(renderer)
      }
    }

    div(className := bss.grid.row + " detail-viewer full-height"
      , EntityDetailView(bss.grid.col5)
      , child <-- store.map { st =>
        selectedEntity = st.selectedSET
        div(className := bss.grid.col7 + " detail-preview full-height full-width"
          , selectedEntity.map(_.createPreview()).getOrElse("")
          //, canvas(id := "detailCanvas"
          //, update --> updateCanvas
        )
      }
    )

  }
}
