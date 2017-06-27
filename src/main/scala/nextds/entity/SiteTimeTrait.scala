package nextds.entity

import java.time.LocalDate
import java.time._
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.temporal.ChronoField
import java.util.Date

/**
  * Created by pascal.mengelt on 27.06.2017.
  * it has no template - so it is a special Comp.
  */
case class TimingComp(siteInfo: SiteEntityInfo
                      , weekDays: WeekDays = WeekDays(),
                      play: Boolean = false,
                      dayPeriod: DayPeriod = DayPeriod(),
                      timePeriod: TimePeriod = TimePeriod()
                 ) extends SiteEntityTrait
  with TimeTrait {
  def levelType: LevelType = TIME
}

case class WeekDays(mon: Boolean = true, tue: Boolean = true, wed: Boolean = true,
                    thu: Boolean = true, fri: Boolean = true, sat: Boolean = true, sun: Boolean = true)


case class DayPeriod(from: Option[LocalDate] = None, to: Option[LocalDate] = None) {

  require(from.isEmpty || to.isEmpty || !to.get.isBefore(from.get)
    , s"${DayPeriod.fromMustBeSmaller} ($from <= $to)")
}

object DayPeriod extends DateTimeHelper {
  val fromMustBeSmaller = "The from Day must be smaller or equal than the to Day!"

  def apply(fromDay: String, toDay: String): DayPeriod =
    DayPeriod(Some(toDayDate(fromDay)), Some(toDayDate(toDay)))
}


case class TimePeriod(from: Option[LocalTime] = None, to: Option[LocalTime] = None) extends DateTimeHelper {

  require(from.isEmpty || to.isEmpty || to.get.isAfter(from.get)
    , s"The from Time must be smaller than the to Time! ($from < $to)")

}

object TimePeriod
  extends DateTimeHelper {
  def apply(fromStr: String, toStr: String): TimePeriod =
    TimePeriod(Some(toTime(fromStr)), Some(toTime(toStr)))

}


/**
  * Created by pascal.mengelt on 09.03.2015.
  *
  */
trait DateTimeHelper {
  val dayDatePattern = "yyyy-MM-dd"
  val dayDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern(dayDatePattern)
  val timePattern = "HH:mm"
  val timeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern(timePattern)
  val timeWithSecondsPattern = "HH:mm:ss"
  val timeWithSecondsFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern(timeWithSecondsPattern)
  val dayNotCorrectFormat = s"Date must have the format $dayDatePattern!"
  val dayMustNotBeBlank = "Date must not be blank!"
  val timeMustNotBeBlank = "Time must not be blank!"

  def toDayDate(dayStr: String): LocalDate = {
    requireNotBlank(dayStr, dayMustNotBeBlank)
    val dateLength = 10 // take only "yyyy-MM-dd"
    require(dayStr.length >= dateLength, dayNotCorrectFormat)

    LocalDate.parse(dayStr.trim.take(dateLength))
  }

  private def requireNotBlank(dayStr: String, msg: String) = {
    require(dayStr != null && dayStr.trim.nonEmpty, msg)
  }

  def toTime(timeStr: String): LocalTime = {
    requireNotBlank(timeStr, timeMustNotBeBlank)
    LocalTime.from(timeFormatter.parse(timeStr.trim))
  }

  def dayStrFromDate(day: LocalDate): String = Option(day) match {
    case Some(d) => dayDateFormatter.format(d)
    case None => throw new IllegalArgumentException(dayMustNotBeBlank)
  }

  def fromDayDate(day: LocalDate): String = Option(day) match {
    case Some(d) => dayDateFormatter.format(d)
    case None => throw new IllegalArgumentException(dayMustNotBeBlank)
  }

  def fromTime(time: LocalTime): String = Option(time) match {
    case Some(t) => timeFormatter.format(t)
    case None => throw new IllegalArgumentException(timeMustNotBeBlank)
  }

  def toDate(ldt: LocalDateTime): Date =
    Date.from(ldt.atZone(ZoneId.systemDefault()).toInstant)

  def fromDate(date: Date): LocalDateTime =
    LocalDateTime.ofInstant(date.toInstant, ZoneId.systemDefault())

  def getLocalTime(date: Date): Option[LocalTime] = {
    if (date == null)
      Option.empty
    else {
      val instant = Instant.ofEpochMilli(date.getTime).atZone(ZoneId.systemDefault())
      Option(LocalTime.of(instant.get(ChronoField.HOUR_OF_DAY), instant.get(ChronoField.MINUTE_OF_HOUR)))
    }
  }

  def getLocalDate(date: Date): Option[LocalDate] = {
    if (date == null)
      Option.empty
    else {
      val instant = Instant.ofEpochMilli(date.getTime)
      Option(LocalDateTime.ofInstant(instant, ZoneId.systemDefault()).toLocalDate)
    }
  }

  def getDateFromDay(localDate: LocalDate): Date = {
    val instant = localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant
    Date.from(instant)
  }

  // for testing
  private[entity] def now = LocalDate.now()

  def getDateFromTime(localTime: LocalTime): Date = {
    val instant = LocalDateTime.of(now, localTime).atZone(ZoneId.systemDefault()).toInstant
    Date.from(instant)
  }

  def getDateFrom(localDate: LocalDate, localTime: LocalTime): Date = {
    val instant = LocalDateTime.of(localDate, localTime).atZone(ZoneId.systemDefault()).toInstant
    Date.from(instant)
  }

  def localDateTimeFromInstant(instant: Instant): LocalDateTime =
    LocalDateTime.ofInstant(instant, ZoneId.systemDefault())

  def localDateTimeStrFrom(localDateTime: LocalDateTime): SiteIdent =
    localDateTime.format(DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM))

  def todayStr: String = fromDayDate(now)

  def tomorrowStr: String = fromDayDate(now.plusDays(1L))

}

object DateTimeHelper extends DateTimeHelper

