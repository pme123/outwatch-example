package nextds.server.control

import nextds.entity.{TimingConf, _}

/**
  * Created by pascal.mengelt on 27.06.2017.
  */
object TimingCreator {
  import SitesRepo._
  import TimingComp._

  private val site = allSites().head

  lazy val timingComps =
      Seq(
        weekend(site)
        , weekdays(site)
        , saturday(site)
        , sunday(site)
        , monday(site)
        , tuesday(site)
        , wednesday(site)
        , thursday(site)
        , friday(site)
        , TimingComp(site, "09:00h-12:00h", TimePeriod("09:00","12:00"))
        , TimingComp(site, "14:00h-19:45h", TimePeriod("14:00","19:45"))
        , TimingComp(site, "24.12.2017 - 31.12.2017", DayPeriod("2017-12-24","2017-12-31"))
      )

  lazy val timingConfs =
    Seq(
      TimingConf(site, "Weekends", Seq(weekend(site)))
      ,  TimingConf(site, "Weekdays", Seq(weekdays(site)))
      ,  TimingConf(site, "Monday and Friday", Seq(monday(site), friday(site)))
      ,  TimingConf(site, "Opening Hours", Seq(weekdays(site), timingComps(9), timingComps(10)))
      ,  TimingConf(site, "Christmas Week", Seq(timingComps.last))
    )
}
