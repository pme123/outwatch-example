package nextds.client.components

import nextds.client.entity._
import nextds.entity.FILTER_TAG
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
object FilterTagCard {

  def apply(uiEntity: UIFilterTagConf)(implicit store: ReduxStore[State, Action]): VNode = {
    val entity = uiEntity.siteEntity

    def entityIcon()(implicit store: ReduxStore[State, Action]): VNode =
      div(className := css.entityCardIcon
        , css.siteTypeIcon(FILTER_TAG))

    def entityIdent: VNode =
      div(className := css.entityCardIdent
        , click(Edit(uiEntity)) --> store
        , entity.ident)

    val selSET = store
      .map(_.selectedSET
        .exists(_.ident == entity.ident)
      )

    div(className := bss.grid.col2
      , div(id := entity.ident
        , className := css.entityCardLI(FILTER_TAG)
        , selected <-- selSET
        , entityIcon()
        , entityIdent
        , div(className := css.entityCardTitle, uiEntity.htmlCondition)
      ))
  }

}

