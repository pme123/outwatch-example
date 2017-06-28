package nextds.server.boundary

import cats.data.{NonEmptyList, Validated}
import nextds.entity._
import nextds.server.control.FilterTagCreator

import scala.util.Try

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
object FilterTagBoundary {


  def filterTags(): FilterTags = FilterTagCreator.filterTags

  def filterTags(fc: FilterCond): Validated[NonEmptyList[String], NonEmptyList[FilterTag]] = FilterTagCreator.filterTags(fc)

  def filterTagConfs(): Seq[FilterTagConf] = FilterTagCreator.filterTagConfs


}
