package nextds.server.boundary

import cats.data.{NonEmptyList, Validated}
import nextds.entity._
import nextds.server.control.{FilterTagCreator, TimingCreator}

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
object TimingBoundary {


  def timingComps(): Seq[TimingComp] = TimingCreator.timingComps

  def timingConfs(): Seq[TimingConf] = TimingCreator.timingConfs


}
