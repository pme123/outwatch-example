package nextds.server.control

import nextds.entity.{FilterTagConf, FilterTags, TagGroup}

/**
  * Created by pascal.mengelt on 23.06.2017.
  */
object FilterTagCreator {

  import SitesCreator._

  lazy val createFilterTags =
    FilterTags(
      Seq(
        TagGroup(allSites.head, "Language")
          .addChildren("DE", "FR", "IT", "EN")
        , TagGroup(allSites.last, "Store")
          .addChildren("MGA", "ORP", "CPF", "NJR")
      ))

  lazy val createFilterTagConfs =
    Seq(
      FilterTagConf(allSites.last, "DE AND (MGA OR CPF)", Seq("DE", "MGA", "CPF").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "(EN AND ORP) OR NJR)", Seq("EN", "ORP", "NJR").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "EN", Seq("EN").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "DE", Seq("DE").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "FR", Seq("FR").map(createFilterTags.filterTag))
      , FilterTagConf(allSites.last, "IT", Seq("IT").map(createFilterTags.filterTag))
    )
}
