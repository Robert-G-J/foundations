package exercises.action

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate}

import exercises.action.imperative.UserCreationExercises.validDateOfBirthFormatter
import org.scalacheck.{Arbitrary, Gen}

object DateGenerator {
  val dateGen: Gen[LocalDate] =
    Gen
      .choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
      .map(LocalDate.ofEpochDay)

  implicit val localDateArb: Arbitrary[LocalDate] =
    Arbitrary(dateGen)

  val instantGen: Gen[Instant] =
    for {
      seconds <- Gen.choose(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
      nano    <- Gen.choose(0, 1000_000_000L)
    } yield Instant.ofEpochSecond(seconds, nano)

  implicit val instantArb: Arbitrary[Instant] =
    Arbitrary(instantGen)

  val stringFromDateGen: (DateTimeFormatter) => Gen[String] = (formatter: DateTimeFormatter) =>
    dateGen.map(formatter.format)
}
