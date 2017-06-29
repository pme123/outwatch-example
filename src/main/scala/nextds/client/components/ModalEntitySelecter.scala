package nextds.client.components

import nextds.client.entity._
import outwatch.dom._
import outwatch.dom.helpers.InputEvent

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
object ModalEntitySelecter {


  def apply()(implicit store: ReduxStore[State, Action]): VNode = {

    val content = store.map(_.linkTo
      .map { lt =>
        val ui = lt.fromEntity
        val entity = ui.siteEntity

        val selectLinkTo: VNode = select(id := "linkTo"
          , className := "form-control"
          , change((ev: InputEvent) => LinkTo(ui, Some(ev.target.value))) --> store
          , children <-- store
            .map(s => s.siteModel)
            .map(sm =>
              ui.linkToType.map(t =>
                sm.uiSiteEntities(entity.levelType, t)
                  .uiSiteEntities
                  .map(e =>
                    option(e.siteEntity.ident)))
                .getOrElse(Seq())
            )
        )
        div(className := "modal-content"
          , div(className := "modal-header"
            , button(`type` := "button", className := "close", data.dismiss := "modal"
              , Icon.close)
            , h4(className := "modal-title"
              , s"Link ${entity.label}: ${entity.ident}")
          ), div(className := "modal-body"
            , div(className := "form-group"
              , label(
                "Link to:")
              , selectLinkTo))
          , div(className := "modal-footer"
            , button(`type` := "button", className := "btn btn-default", data.dismiss := "modal"
              , click(DoLinking) --> store
              , "Link")
            , button(`type` := "button", className := "btn btn-default", data.dismiss := "modal"
              , "Close")
          )
        )


      }.getOrElse(""))

    div(id := "modalDialog",
      className := "modal fade"
      , Attribute("role", "dialog")
      , div(className := "modal-dialog"
        , child <-- content))
  }
}
