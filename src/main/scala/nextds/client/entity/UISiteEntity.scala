package nextds.client.entity

import nextds.entity._
import outwatch.dom._

/**
  * Created by pascal.mengelt on 28.05.2017.
  */
trait UISiteEntity
  extends UIElements {
  def siteEntity: SiteEntityTrait

  def levelType: LevelType = siteEntity.levelType
  def siteType: SiteType = siteEntity.siteType

  def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
    Seq(
      inputText("Title", siteEntity.title, siteEntity.maybeTitle)
      , inputTextarea("Description", siteEntity.descr, siteEntity.maybeDescr)
    )
  }
}

trait UIPlayer extends UISiteEntity {
  def siteEntity: PlayerTrait

}

trait UILayout extends UISiteEntity {
  def siteEntity: LayoutTrait

  override def parameterEdit()(implicit store: ReduxStore[State, Action]): Seq[VNode] = {
   super.parameterEdit() ++
     Seq(
       inputNumber("From Left"
         , siteEntity.screenRegion.fromLeft.toString
         , siteEntity.maybeScreenRegion.map(_.fromLeft))
       ,inputNumber("From Top"
         , siteEntity.screenRegion.fromTop.toString
         , siteEntity.maybeScreenRegion.map(_.fromLeft))
       ,inputNumber("Width"
         , siteEntity.screenRegion.width.toString
         , siteEntity.maybeScreenRegion.map(_.width))
       ,inputNumber("From Left"
         , siteEntity.screenRegion.height.toString
         , siteEntity.maybeScreenRegion.map(_.height))
      )
  }
}

// only for config
trait UIRegion extends UISiteEntity {
  def siteEntity: RegionTrait
}

trait UIPlaylist extends UISiteEntity {
  def siteEntity: PlaylistTrait
}

trait UIMedium extends UISiteEntity {
  def siteEntity: MediumTrait
}
