package nextds.client.components

import nextds.client.entity._
import nextds.entity.{FilterTagConf, SiteEntityIdent, SiteEntityTrait, TAG_FILTER}
import org.scalajs.dom.DragEvent
import outwatch.dom._

/**
  * Created by pascal.mengelt on 15.06.2017.
  */
object FilterTagCard {

  def apply(filterTagConf: UIFilterTagConf)(implicit store: ReduxStore[State, Action]): VNode = {
    div(className := bss.grid.col3
      , div(id := filterTagConf.ident
        , className := css.entityCardLI(TAG_FILTER)
        , entityIcon()
        , entityIdent(filterTagConf.ident)
        , div(className := css.entityCardTitle, filterTagConf.htmlCondition)
      ))
  }

  def entityIcon()(implicit store: ReduxStore[State, Action]): VNode =
    div(className := css.entityCardIcon
      , css.siteTypeIcon(TAG_FILTER))

  def entityIdent(ident: SiteEntityIdent): VNode =
    div(className := css.entityCardIdent
      , ident)

}

