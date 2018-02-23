package com.acme.protocolchaos.generators

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import org.scalacheck.Gen

object DateGenerators {

  implicit class DateSegment(n: Int) {
    def toDateSegment: String =
      if (n < 10) s"0$n" else s"$n"
  }

  implicit class LocalDateTimeString(d: LocalDateTime) {
    def toISOString: String =
      s"${d.getYear}-${d.getMonthValue.toDateSegment}-${d.getDayOfMonth.toDateSegment}T${d.getHour.toDateSegment}:${d.getMinute.toDateSegment}:${d.getSecond.toDateSegment}Z"
  }

  private def validStringISODate(maxDate: LocalDateTime): Gen[String] = {
    val millisBetween = ChronoUnit.MILLIS.between(LocalDateTime.now(), maxDate)
    val millisGen = Gen.choose(0, millisBetween).sample.get
    val randDate = LocalDateTime.now().plus(millisGen, ChronoUnit.MILLIS)
    randDate.toISOString
  }

  private def dateSegmentGen(min: Int, max: Int) =
    Gen.choose(min, max).map(_.toDateSegment)

  // We avoid initializing a new date due to exceptions
  private val distantStringISODate: Gen[String] = for {
    year <- Gen.choose(-9000, 9000)
    month <- dateSegmentGen(0, 12)
    day <- dateSegmentGen(0, 31)
    hours <- dateSegmentGen(0, 24)
    minutes <- dateSegmentGen(0, 59)
    seconds <- dateSegmentGen(0, 59)
  } yield s"$year-$month-${day}T$hours:$minutes:${seconds}Z"

  val randomDate: Gen[String] = Gen.oneOf(
    validStringISODate(LocalDateTime.now().plusDays(8)),
    distantStringISODate,
    Gen.alphaNumStr
  )

}
